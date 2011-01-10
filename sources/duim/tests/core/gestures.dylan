Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Gesture tests

define method test-mouse-gesture
    (button :: <symbol>, #key modifiers = #[]) => ()
  let gesture = #f;
  let pointer = find-test-pointer();
  check-true("make(<gesture>) returns instance of <gesture>",
             begin
               gesture := make(<gesture>, button: button, modifiers: modifiers);
               instance?(gesture, <gesture>)
             end);
  check-true("event matches gesture",
             event-matches-gesture?(make(<button-release-event>,
                                         sheet: make-test-pane(<null-pane>),
                                         pointer: pointer,
                                         button: select (button)
                                                   #"left"   => $left-button;
                                                   #"right"  => $right-button;
                                                   #"middle" => $middle-button;
                                                 end,
                                         modifier-state: apply(make-modifier-state, modifiers)),
                                    gesture))
end method test-mouse-gesture;

define test mouse-gestures-test ()
  do(method (modifiers)
       test-mouse-gesture(#"left",   modifiers: modifiers);
       test-mouse-gesture(#"middle", modifiers: modifiers);
       test-mouse-gesture(#"right",  modifiers: modifiers);
     end,
     vector(#[],
            #[#"shift"],
            #[#"control"],
            #[#"alt"]))
end test mouse-gestures-test;

define test keyboard-gestures-test ()
end test keyboard-gestures-test;


/// Define the gestures test suite

define suite duim-gestures-suite ()
  test mouse-gestures-test;
  test keyboard-gestures-test;
end suite duim-gestures-suite;
