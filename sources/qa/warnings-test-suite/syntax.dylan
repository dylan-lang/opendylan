Module:       warnings-test-suite
Synopsis:     A test suite for compiler warnings
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $list-with-missing-item    = #[1, 2,];
define constant $list-with-missing-comma   = #[1, 2 3];
define constant $list-with-missing-bracket = #[1, 2, 3;

define function missing-semicolon
    () => (x :: <integer>)
  let x = 10
  x
end function missing-semicolon;

define function invalid-block
    () => ()
  block
    10 * 10
  end
end function invalid-block;

define function mismatched-end-clause
    () => ()
end class mismatched-end-clause;

ignore(missing-semicolon,
       invalid-block,
       mismatched-end-clause);

/* Unclosed comment
