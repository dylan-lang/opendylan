Module:       system-test-suite
Synopsis:     System library test suite
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Operating system tests

define operating-system constant-test $architecture-little-endian? ()
  //---*** Fill this in...
end constant-test $architecture-little-endian?;

define operating-system constant-test $os-name ()
  //---*** Fill this in...
end constant-test $os-name;

define operating-system constant-test $os-variant ()
  //---*** Fill this in...
end constant-test $os-variant;

define operating-system constant-test $os-version ()
  //---*** Fill this in...
end constant-test $os-version;

define operating-system constant-test $platform-name ()
  //---*** Fill this in...
end constant-test $platform-name;

define operating-system constant-test $machine-name ()
  //---*** Fill this in...
end constant-test $machine-name;


/// Operating System functions

define operating-system function-test login-name ()
  //---*** Fill this in...
end function-test login-name;

define operating-system function-test login-group ()
  //---*** Fill this in...
end function-test login-group;

define operating-system function-test owner-name ()
  //---*** Fill this in...
end function-test owner-name;

define operating-system function-test owner-organization ()
  //---*** Fill this in...
end function-test owner-organization;

define operating-system function-test run-application ()
  //---*** Fill this in...
end function-test run-application;

define operating-system function-test load-library ()
  //---*** Fill this in...
end function-test load-library;


// Application startup handling

define operating-system function-test application-name ()
  //---*** Fill this in...
end function-test application-name;

define operating-system function-test application-filename ()
  //---*** Fill this in...
end function-test application-filename;

define operating-system function-test application-arguments ()
  //---*** Fill this in...
end function-test application-arguments;

define operating-system function-test tokenize-command-string ()
  //---*** Fill this in...
end function-test tokenize-command-string;

define operating-system function-test command-line-option-prefix ()
  //---*** Fill this in...
end function-test command-line-option-prefix;

define operating-system function-test exit-application ()
  //---*** Fill this in...
end function-test exit-application;

define operating-system function-test register-application-exit-function ()
  //---*** Fill this in...
end function-test register-application-exit-function;


// Environment variables

define operating-system function-test environment-variable ()
  //---*** Fill this in...
end function-test environment-variable;

define operating-system function-test environment-variable-setter ()
  //---*** Fill this in...
end function-test environment-variable-setter;

define operating-system function-test tokenize-environment-variable ()
  //---*** Fill this in...
end function-test tokenize-environment-variable;
