Module:       duim-gui-test-suite
Author:       Andy Armstrong, Shri Amit
Synopsis:     An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A simple harness for all of the tests

define variable *test-frames* = make(<stretchy-vector>);

define variable *use-threads?* :: <boolean> = #t;

define method find-test-class (class :: <class>)
  block (return)
    for (test in *test-frames*)
      if (test[0] = class)
        return(test)
      end
    end
  end
end method find-test-class;

define method install-test 
    (frame-class :: <class>, title :: <string>)
  let test = find-test-class(frame-class);
  if (test)
    test[1] := title;
  else
    add!(*test-frames*, vector(frame-class, title))
  end;
  frame-class
end method install-test;

define method start-test-frame 
    (frame :: <frame>, class :: <class>, 
     #rest args,
     #key frame-manager: framem)
 => ()
  with-busy-cursor (frame)
    let test = find-test-class(class);
    let frame-class = test[0];
    let title = test[1];
    local method create-test-frame
	      (#key owner) => ()
	    with-abort-restart ()
	      let test-frame
	        = if (test)
		    apply(make, frame-class, title: title, owner: owner, args)
		  else
		    apply(make, class, title: "Test", owner: owner, args)
		  end;
	      start-frame(test-frame)
	    end
	  end method create-test-frame;
    if (*use-threads?*)
      make(<thread>,
	   name: title,
	   function: create-test-frame)
    else
      create-test-frame(owner: frame)
    end
  end
end method start-test-frame;

define method sorted-test-frames ()
  sort(*test-frames*,
       test: method (test1, test2)
               test1[1] < test2[1]
             end)
end method sorted-test-frames;


/// A frame to show them in

define frame <tests-harness> (<simple-frame>)
  pane update (frame)
    make(<push-button>,
	 label: "Update",
	 documentation: "Update the list of available tests",
	 activate-callback: update-tests-harness);
  pane tests (frame)
    begin
      let frames = sorted-test-frames();
      make(<list-box>,
	   documentation: "Double-click on a test name to run it",
	   items: frames,
	   lines: size(frames),
	   label-key: second,
	   value-key: first,
	   activate-callback: method (sheet :: <sheet>)
				let frame = sheet-frame(sheet);
				let test = gadget-value(sheet);
				start-test-frame(frame, test)
			      end)
    end;
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.update;
      frame.tests;
    end;
  layout (frame) frame.main-layout;
end frame <tests-harness>;

define method update-tests-harness 
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  gadget-items(frame.tests) := sorted-test-frames()
end method update-tests-harness;

define method start-tests 
    () => (status-code :: <integer>)
  let frame = make(<tests-harness>, title: "Tests");
  frame-input-focus(frame) := frame.tests;
  start-frame(frame) | 0
end method start-tests;


/// A useful graphic class

define class <ellipse-pane> (<drawing-pane>)
  slot ellipse-foreground :: <color> = $red,
    init-keyword: foreground:;
end class <ellipse-pane>;

define method handle-repaint
    (sheet :: <ellipse-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  let (left, top, right, bottom) = box-edges(sheet);
  let center-x = floor/(right + left - 1, 2);
  let center-y = floor/(top + bottom - 1, 2);
  let x-radius = center-x - left;
  let y-radius = center-y - top;
  with-drawing-options (medium, brush: ellipse-foreground(sheet))
    draw-ellipse(medium, center-x, center-y, x-radius, 0, 0, y-radius)
  end
end method handle-repaint;


/// A useful text class

define class <text-pane> (<drawing-pane>)
  slot pane-text-style :: <text-style> = $default-text-style,
    init-keyword: text-style:;
  keyword min-width:  = 200;
  keyword min-height: = 50;
  keyword max-height: = 50;
end class <text-pane>;

define method handle-repaint
    (pane :: <text-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  with-drawing-options (medium, text-style: pane-text-style(pane))
    draw-text(medium, "abcdefgABCDEFG", 0, 0, align-y: #"top")
  end
end method handle-repaint;
