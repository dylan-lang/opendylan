Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant testing

define common-extensions constant-test $unsupplied ()
  //---** What can we do here?
end constant-test $unsupplied;

define common-extensions constant-test $unfound ()
  //---** What can we do here?
end constant-test $unfound;


/// Streams

define streams-protocol constant-test *standard-input* ()
  //---*** Fill this in!
end constant-test *standard-input*;

define streams-protocol constant-test *standard-output* ()
  //---*** Fill this in!
end constant-test *standard-output*;

define streams-protocol constant-test *standard-error* ()
  //---*** Fill this in!
end constant-test *standard-error*;

define streams-protocol constant-test *debug-output* ()
  //---*** Fill this in!
end constant-test *debug-output*;
