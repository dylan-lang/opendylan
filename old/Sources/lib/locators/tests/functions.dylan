Module:    locators-test-suite
Synopsis:  Locators library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Function test cases

define locators function-test wild-locator? ()
  //---*** Fill this in...
end function-test wild-locator?;

define locators function-test locators-match? ()
  //---*** Fill this in...
end function-test locators-match?;

define locators function-test absolute-locator? ()
  //---*** Fill this in...
end function-test absolute-locator?;

define locators function-test relative-locator? ()
  //---*** Fill this in...
end function-test relative-locator?;

define locators function-test translate-locator ()
  //---*** Fill this in...
end function-test translate-locator;

define locators function-test merge-locators ()
  //---*** Fill this in...
end function-test merge-locators;

define constant $abbreviate-tests
 = #[#["a",         "a",         "a"],
     #["a",         "b",         "a"],
     #["a/",        "a/",        "./"],
     #["a/b",       "a/",        "b"],
     #["a/./b",     "a/",        "b"],
     #["b/c",       "a/",        "b/c"],
     #["/a/b/c",    "/a/b/",     "c"],
     #["/a/b/c",    "/a/",       "b/c"],
     #["/a/b/",     "/a/b/c/",   "../"],
     #["/a/b/c",    "/d/e/f/",   "../../../a/b/c"],
     #["h:/a/b/c",  "h:/a/b/",   "c"],
     #["h:/a/b/c",  "i:/a/b/c/", "h:/a/b/c"],
     #["host:/a/b", "/a/b/c",    "host:/a/b"]];

define constant $url-abbreviate-tests
  = #[#["http://www.functionalobjects.com/hello.html",
        "http://www.functionalobjects.com/hello.html",
        "hello.html"]];

define locators function-test abbreviate-locator ()
  for (class in vector(<microsoft-locator>, <posix-locator>, <url-locator>))
    for (test in $abbreviate-tests)
      let file1 = test[0];
      let file2 = test[1];
      let file3 = test[2];
      check-equal(format-to-string("%s: abbreviate(%s, %s) = %s",
				   class, file1, file2, file3),
		  abbreviate-locator(as(class, file1),
				     as(class, file2)),
		  as(class, file3))
    end
  end;
  for (test in $url-abbreviate-tests)
    let file1 = test[0];
    let file2 = test[1];
    let file3 = test[2];
    check-equal(format-to-string("%s: abbreviate(%s, %s) = %s",
				 <url-locator>, file1, file2, file3),
		abbreviate-locator(as(<url-locator>, file1),
				   as(<url-locator>, file2)),
		as(<url-locator>, file3))
  end
end function-test abbreviate-locator;

define constant $simplify-tests
 = #[#["a",         "a"],
     #["a.t",       "a.t"],
     #["./",        "./"],	// This one is confusing, but correct
     #["./a.t",     "a.t"],
     #["a/./b.t",   "a/b.t"],
     #["a/../b.t",  "b.t",    #"not-posix"],
     #["/a/../b.t", "/b.t",   #"not-posix"]];

define locators function-test simplify-locator ()
  for (class in vector(<microsoft-locator>, <posix-locator>, <url-locator>))
    for (test in $simplify-tests)
      let file1 = test[0];
      let file2 = test[1];
      let not-posix? = size(test) > 2 & test[2] == #"not-posix";
      check-equal(format-to-string("%s: simplify(%s) = %s",
				   class, file1, file2),
		  simplify-locator(as(class, file1)),
		  if (not-posix? & class == <posix-locator>)
		    as(class, file1)
		  else
		    as(class, file2)
		  end)
    end;
    check-condition("simplify-locator(\"/../../\") errors",
		    <error>,
		    simplify-locator(as(class, "/../../")))
  end;
end function-test simplify-locator;

define locators function-test override-locator ()
  //---*** Fill this in...
end function-test override-locator;

define locators function-test default-locator ()
  //---*** Fill this in...
end function-test default-locator;

define locators function-test make-abstract-host-table ()
  //---*** Fill this in...
end function-test make-abstract-host-table;

define locators function-test clear-abstract-host-table ()
  //---*** Fill this in...
end function-test clear-abstract-host-table;

define locators function-test add-abstract-host ()
  //---*** Fill this in...
end function-test add-abstract-host;

define locators function-test remove-abstract-host ()
  //---*** Fill this in...
end function-test remove-abstract-host;

define locators function-test locator-scheme ()
  //---*** Fill this in...
end function-test locator-scheme;

define locators function-test locator-host ()
  //---*** Fill this in...
end function-test locator-host;

define locators function-test locator-port ()
  //---*** Fill this in...
end function-test locator-port;

define locators function-test locator-user-id ()
  //---*** Fill this in...
end function-test locator-user-id;

define locators function-test locator-password ()
  //---*** Fill this in...
end function-test locator-password;

define locators function-test locator-volume ()
  //---*** Fill this in...
end function-test locator-volume;

define locators function-test locator-directory ()
  //---*** Fill this in...
end function-test locator-directory;

define locators function-test locator-base ()
  //---*** Fill this in...
end function-test locator-base;

define locators function-test locator-type ()
  //---*** Fill this in...
end function-test locator-type;

define locators function-test locator-version ()
  //---*** Fill this in...
end function-test locator-version;

define locators function-test locator-search-keys ()
  //---*** Fill this in...
end function-test locator-search-keys;

define locators function-test locator-transfer-type ()
  //---*** Fill this in...
end function-test locator-transfer-type;

define locators function-test locator-prefix ()
  //---*** Fill this in...
end function-test locator-prefix;

define locators function-test locator-name ()
  //---*** Fill this in...
end function-test locator-name;

define locators function-test locator-extension ()
  //---*** Fill this in...
end function-test locator-extension;

define locators function-test locator-suffix ()
  //---*** Fill this in...
end function-test locator-suffix;

define locators function-test path-elements ()
  //---*** Fill this in...
end function-test path-elements;
