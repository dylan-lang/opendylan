Module:       win32-duim-gui-test-suite
Author:       Andy Armstrong, Scott McKay
Synopsis:     An interactive test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Images for use by the tests

define variable *bitmaps-initialized?* :: <boolean> = #f;

define macro initialize-bitmap
  { initialize-bitmap(?bitmap:name, ?resource-id:expression) }
    => { let _id     = as(<byte-string>, ?resource-id);
	 let _bitmap = read-image-as(<win32-bitmap>, _id, #"bitmap",
				     //--- Strictly speaking, we shouldn't need to do this
				     width: 16, height: 16);
	 when (_bitmap)
	   ?bitmap := _bitmap
	 end }
end macro initialize-bitmap;

define macro initialize-icon
  { initialize-icon(?icon:name, ?resource-id:expression) }
    => { let _id   = as(<byte-string>, ?resource-id);
	 let _icon = read-image-as(<win32-icon>, _id, #"small-icon");
	 when (_icon)
	   ?icon := _icon
	 end }
end macro initialize-icon;

define variable $cut-icon   = #f;
define variable $copy-icon  = #f;
define variable $paste-icon = #f;

define variable $wizard-icon = #f;

define variable $location-bitmap = #f;
define variable $prompt-bitmap   = #f;
define variable $values-bitmap   = #f;

define function initialize-images ()
  unless (*bitmaps-initialized?*)
    initialize-icon($cut-icon,  "CUT");
    initialize-icon($copy-icon, "COPY");
    initialize-icon($paste-icon, "PASTE");
    initialize-icon($wizard-icon, "WIZARD");
    initialize-bitmap($location-bitmap, "CURRENTLOCATION");
    initialize-bitmap($prompt-bitmap,   "PROMPT");
    initialize-bitmap($values-bitmap,   "VALUES");
    *bitmaps-initialized?* := #t
  end
end function initialize-images;

initialize-images();


/// A simple harness for all of the tests

define variable *test-frames* = make(<stretchy-vector>);

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
 => (thread :: <thread>)
  with-busy-cursor (frame)
    let test = find-test-class(class);
    let frame-class = test[0];
    let title = test[1];
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
    make(<list-box>,
	 documentation: "Double-click on a test name to run it",
         items: sorted-test-frames(),
         label-key: second,
         value-key: first,
         activate-callback: method (sheet :: <sheet>)
                              let frame = sheet-frame(sheet);
                              let test = gadget-value(sheet);
                              start-test-frame(frame, test)
                            end);
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
  constant slot ellipse-foreground :: <color> = $red,
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
  constant slot pane-text-style :: <text-style> = $default-text-style,
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
