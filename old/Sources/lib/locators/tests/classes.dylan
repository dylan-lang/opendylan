Module:    locators-test-suite
Synopsis:  Locators library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class test cases

define sideways method class-test-function
    (class :: subclass(<locator>)) => (function :: <function>)
  test-locator-class
end method class-test-function;

define function test-path-class
    (class :: subclass(<path>),
     #key name, abstract?, instantiable?, #all-keys) => ()
  //---*** Fill this in...
end function test-path-class;

define sideways method class-test-function
    (class :: subclass(<path>)) => (function :: <function>)
  test-path-class
end method class-test-function;


/// Locator class tests

define method test-locator-class
    (class :: subclass(<locator>),
     #key name, instantiable?, abstract?, #all-keys) => ()
  //---*** Fill this in...
end method test-locator-class;

define constant $microsoft-filenames
  = #["/users/dylan/hello/",
      "c:/Program Files/Functional Objects/Developer/bin/functional-dylan.exe",
      "//machine/users/dylan/test.dylan"];

define method test-locator-class
    (class == <microsoft-locator>,
     #key name, instantiable?, abstract?, #all-keys) => ()
  for (filename in $microsoft-filenames)
    let locator = as(<microsoft-locator>, filename);
    check-true(format-to-string
		 ("as(<microsoft-locator>, %=) returns valid locator",
		  filename),
	       instance?(locator, <microsoft-locator>));
    check-true(format-to-string("%s insensitive to case", filename),
	       locator = as(<microsoft-locator>, as-uppercase(filename)));
    let alternate-filename
      = map(method (char :: <character>)
	      if (char == '/') '\\' else char end
	    end,
	    filename);
    check-true(format-to-string("locator %= = locator %=",
				filename, alternate-filename),
	       locator = as(<microsoft-locator>, alternate-filename));
  end
end method test-locator-class;
