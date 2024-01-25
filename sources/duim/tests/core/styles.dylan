Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Color constants

define test test-$foreground ()
  //---*** Fill this in...
end test;

define test test-$background ()
  //---*** Fill this in...
end test;

define test test-$black ()
  //---*** Fill this in...
end test;

define test test-$blue ()
  //---*** Fill this in...
end test;

define test test-$cyan ()
  //---*** Fill this in...
end test;

define test test-$green ()
  //---*** Fill this in...
end test;

define test test-$magenta ()
  //---*** Fill this in...
end test;

define test test-$red ()
  //---*** Fill this in...
end test;

define test test-$white ()
  //---*** Fill this in...
end test;

define test test-$yellow ()
  //---*** Fill this in...
end test;


/// Color classes

define sideways method make-test-instance
    (class == <color>) => (color :: <color>)
  make(<color>, red: 1, green: 0, blue: 0)
end method make-test-instance;

define test test-<color-not-found> ()
  //---*** Fill this in...
end test;

define test test-<color> ()
  //---*** Fill this in...
end test;

define test test-<contrasting-color> ()
  //---*** Fill this in...
end test;

define test test-<dynamic-color> ()
  //---*** Fill this in...
end test;


/// DC tests

define test test-<brush> ()
  //---*** Fill this in...
end test;

define test test-<device-font> ()
  //---*** Fill this in...
end test;

define test test-<image> ()
  //---*** Fill this in...
end test;

define test test-<ink> ()
  //---*** Fill this in...
end test;

define test test-<palette-full> ()
  //---*** Fill this in...
end test;

define test test-<palette> ()
  //---*** Fill this in...
end test;

define test test-<pattern> ()
  //---*** Fill this in...
end test;

define test test-<pen> ()
  //---*** Fill this in...
end test;

define test test-<stencil> ()
  //---*** Fill this in...
end test;

define test test-<text-style> ()
  //---*** Fill this in...
end test;


/// Color functions

define test test-color-ihs ()
  //---*** Fill this in...
end test;

define test test-color-rgb ()
  //---*** Fill this in...
end test;

define test test-color-luminosity ()
  //---*** Fill this in...
end test;

define test test-color? ()
  //---*** Fill this in...
end test;

define test test-contrasting-colors-limit ()
  //---*** Fill this in...
end test;

define test test-make-contrasting-colors ()
  //---*** Fill this in...
end test;

define test test-make-gray-color ()
  //---*** Fill this in...
end test;

define test test-make-ihs-color ()
  //---*** Fill this in...
end test;

define test test-make-rgb-color ()
  //---*** Fill this in...
end test;


/// Text style tests

define test default-text-styles-test ()
  let text-style = $default-text-style;
  let button-text-style = make-text-style("courier", #f, #f, #f, #f);
  let button = make-test-pane(<push-button>, text-style: button-text-style);
  // check-equal("pane text style",
  //             pane-text-style(button), button-text-style);
  let layout = make-test-pane(<row-layout>, children: vector(button));
  let frame
    = make-test-frame(<test-frame>, layout: layout, text-style: text-style);
  let top-sheet = top-level-sheet(frame);
  let medium = sheet-medium(top-sheet);
  check-equal("top-level-sheet default text-style",
              medium-text-style(medium), text-style);
  frame
end test default-text-styles-test;


/// Color tests

define test color-equality-test ()
  let color = make-rgb-color(1, 0, 0);
  check-equal("make-rgb-color = $red", color, $red);
  let color = make-rgb-color(0, 1, 0);
  check-equal("make-rgb-color = $green", color, $green);
  let color = make-rgb-color(0, 0, 1);
  check-equal("make-rgb-color = $blue", color, $blue);
end test color-equality-test;

define test default-colors-test ()
  let foreground = $red;
  let background = $green;
  let button-foreground = $blue;
  let button-background = $black;
  let button
    = make-test-pane(<push-button>,
                     foreground: button-foreground,
                     background: button-background);
  check-equal("pane foreground",
              default-foreground(button), button-foreground);
  check-equal("pane background",
              default-background(button), button-background);
  let layout = make-test-pane(<row-layout>, children: vector(button));
  let frame
    = make-test-frame(<test-frame>,
                      layout: layout,
                      foreground: foreground,
                      background: background);
  let top-sheet = top-level-sheet(frame);
  let medium = sheet-medium(top-sheet);
  check-equal("top-level-sheet default foreground",
              medium-foreground(medium), foreground);
  check-equal("top-level-sheet default background",
              medium-background(medium), background);
  frame
end test default-colors-test;


/// Define the colors test suite

define suite duim-colors-suite ()
  test default-text-styles-test;
  test color-equality-test;
  test default-colors-test;
end suite duim-colors-suite;
