Module:       system-test-suite
Synopsis:     System library test suite
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Locators class test cases

define locators class-test <locator> ()
  //---*** Fill this in...
end class-test <locator>;

define locators class-test <server-locator> ()
  //---*** Fill this in...
end class-test <server-locator>;

define locators class-test <physical-locator> ()
  //---*** Fill this in...
end class-test <physical-locator>;

define locators class-test <directory-locator> ()
  //---*** Fill this in...
end class-test <directory-locator>;

define locators class-test <file-locator> ()
  //---*** Fill this in...
end class-test <file-locator>;

define locators class-test <native-directory-locator> ()
  //---*** Fill this in...
end class-test <native-directory-locator>;

define locators class-test <native-file-locator> ()
  //---*** Fill this in...
end class-test <native-file-locator>;

define locators class-test <locator-error> ()
  //---*** Fill this in...
end class-test <locator-error>;
  

/// Web locator classes

define sideways method make-test-instance
    (class :: subclass(<server-url>))
 => (instance :: <server-url>)
  make(class, host: "www.functionalobjects.com")
end method make-test-instance;

define locators class-test <web-locator> ()
  //---*** Fill this in...
end class-test <web-locator>;

define constant $valid-urls
  = #["http://www.functionalobjects.com/devtools/dylan",
      "http://www.functionalobjects.com/devtools/dylan/Welcome.html",
      "http://www.functionalobjects.com/devtools/dylan/Welcome.html#register",
      "http://www.functionalobjects.com:2000/devtools/dylan/Welcome.html",
      "http://www.functionalobjects.com/bugs.asp?date=today&severity=5",
      "https://www.functionalobjects.com/secure/shop.asp",
      "ftp://www.functionalobjects.com/pub/incoming",
      "file://host/directory/file.txt",
      "file:///root/directory/file.txt",
      "ftp://www.functionalobjects.com:100/bin",
      "ftp://hello-world.org?andrewa:my-password/pub",
      "mailto:andrewa@functionalobjects.com"];

define locators class-test <url> ()
  for (url :: <string> in $valid-urls)
    check-equal(format-to-string("as(<string>, as(<url>, x)) = x [with x = %=]", url),
		as(<string>, as(<url>, url)),
		url)
  end
end class-test <url>;

define constant $valid-server-urls
  = #["http://www.functionalobjects.com",
      "http://www.functionalobjects.com:2000",
      "https://www.functionalobjects.com",
      "ftp://www.hello-world.org",
      "file://c:"];

define locators class-test <server-url> ()
  for (url :: <string> in $valid-server-urls)
    check-equal(format-to-string("as(<string>, as(<server-url>, x)) = x [with x = %=]", url),
		as(<string>, as(<server-url>, url)),
		url)
  end
end class-test <server-url>;

define locators class-test <http-server> ()
  let server = #f;
  check-equal("make <http-server>",
	      server
		:= make(<http-server>,
			host: "www.functionalobjects.com",
			port: 80,
			username: "andrewa",
			password: "test"),
	      as(<http-server>, "http://www.functionalobjects.com:80?andrewa:test"));
end class-test <http-server>;

define locators class-test <https-server> ()
  let server = #f;
  check-equal("make <http-server>",
	      server
		:= make(<https-server>,
			host: "www.functionalobjects.com",
			port: 80,
			username: "andrewa",
			password: "test"),
	      as(<https-server>, "https://www.functionalobjects.com:80?andrewa:test"));
end class-test <https-server>;

define locators class-test <ftp-server> ()
  //---*** Fill this in...
end class-test <ftp-server>;

define locators class-test <file-server> ()
  //---*** Fill this in...
end class-test <file-server>;

define locators class-test <directory-url> ()
  //---*** Fill this in...
end class-test <directory-url>;

define locators class-test <file-url> ()
  //---*** Fill this in...
end class-test <file-url>;

define locators class-test <file-index-url> ()
  //---*** Fill this in...
end class-test <file-index-url>;

define locators class-test <cgi-url> ()
  //---*** Fill this in...
end class-test <cgi-url>;

define locators class-test <mail-to-locator> ()
  //---*** Fill this in...
end class-test <mail-to-locator>;


/// Locator function test cases

define locators function-test locator-error ()
  //---*** Fill this in...
end function-test locator-error;

define locators function-test supports-open-locator? ()
  //---*** Fill this in...
end function-test supports-open-locator?;

define locators function-test open-locator ()
  //---*** Fill this in...
end function-test open-locator;

define locators function-test supports-list-locator? ()
  //---*** Fill this in...
end function-test supports-list-locator?;

define locators function-test list-locator ()
  //---*** Fill this in...
end function-test list-locator;

define locators function-test locator-host ()
  //---*** Fill this in...
end function-test locator-host;

define locators function-test locator-server ()
  //---*** Fill this in...
end function-test locator-server;

define locators function-test locator-volume ()
  //---*** Fill this in...
end function-test locator-volume;

define locators function-test locator-directory ()
  //---*** Fill this in...
end function-test locator-directory;

define locators function-test locator-relative? ()
  //---*** Fill this in...
end function-test locator-relative?;

define locators function-test locator-path ()
  //---*** Fill this in...
end function-test locator-path;

define locators function-test locator-base ()
  //---*** Fill this in...
end function-test locator-base;

define locators function-test locator-extension ()
  //---*** Fill this in...
end function-test locator-extension;

define locators function-test locator-name ()
  //---*** Fill this in...
end function-test locator-name;

/// Coercion protocols

define locators function-test locator-as-string ()
  //---*** Fill this in...
end function-test locator-as-string;

define locators function-test string-as-locator ()
  //---*** Fill this in...
end function-test string-as-locator;

/// Utilities

define constant $simplify-tests
  = #[#["a",         "a"],
      #["a.t",       "a.t"],
      #["./",        "./"],	// This one is confusing, but correct
      #["./a.t",     "a.t"],
      #["a/./b.t",   "a/b.t"],
      #["../a/b",    "../a/b"],
      #["../a/./b",  "../a/b"]];

define constant $microsoft-simplify-tests
  = #[#["a/../b.t",  "b.t"],
      #["/a/../b.t", "/b.t"]];

define locators function-test simplify-locator ()
  local method test-simplify-locator
	    (class :: subclass(<locator>), info :: <vector>) => ()
	  let file1 = info[0];
	  let file2 = info[1];
	  check-equal(format-to-string("%s: simplify(%=) = %=",
				       class, file1, file2),
		      simplify-locator(as(class, file1)),
		      as(class, file2))
	end method test-simplify-locator;
  for (class in vector(<microsoft-file-system-locator>, <posix-file-system-locator>))
    do(curry(test-simplify-locator, class), $simplify-tests)
  end;
  do(curry(test-simplify-locator, <microsoft-file-system-locator>),
     $microsoft-simplify-tests)
end function-test simplify-locator;

define constant $subdirectory-tests
  = #[#["a/",    #["b"],      "a/b/"],
      #["a/b/",  #["c", "d"], "a/b/c/d/"]];

define constant $microsoft-subdirectory-tests
  = #[#["h:/a/",  #["b"],    "h:/a/b/"],
      #["h:/a/",  #["b"],    "H:/a/b/"],
      #["//h/a/", #["b"],    "//h/a/b/"]];

define locators function-test subdirectory-locator ()
  local method test-subdirectory-locator
	    (class :: subclass(<locator>), info :: <vector>) => ()
	  let directory      = info[0];
	  let subdirectories = info[1];
	  let result         = info[2];
	  check-equal(format-to-string("%s: subdirectory(%=, %=) = %=",
				       class, directory, subdirectories,
				       result),
		      apply(subdirectory-locator,
			    as(class, directory),
			    subdirectories),
		      as(class, result))
	end method test-subdirectory-locator;
  for (class in vector(<microsoft-file-system-locator>, <posix-file-system-locator>))
    do(curry(test-subdirectory-locator, class), $subdirectory-tests)
  end;
  do(curry(test-subdirectory-locator, <microsoft-file-system-locator>),
     $microsoft-subdirectory-tests)
end function-test subdirectory-locator;

define constant $relative-tests
  = #[#["a",         "a",         "a"],
      #["a",         "b",         "a"],
      #["a/",        "a/",        "./"],
      #["a/b",       "a/",        "b"],
      #["a/./b",     "a/",        "b"],
      #["b/c",       "a/",        "../b/c"],
      #["/a/b/c",    "/a/b/",     "c"],
      #["/a/b/c",    "/a/",       "b/c"],
      #["/a/b/",     "/a/b/c/",   "../"],
      #["/a/b/c",    "/d/e/f/",   "../../../a/b/c"]];

define constant $microsoft-relative-tests
  = #[#["h:/a/b/c",  "h:/a/b/",   "c"],
      #["h:/a/b/c/", "h:/a/b/",   "c/"],
      #["h:/a/b/c",  "i:/a/b/c/", "h:/a/b/c"],
      #["h:/a/b/c/", "i:/a/b/c/", "h:/a/b/c/"],
      #["//h/a/b",   "//h/a/",    "b"],
      #["//h/a/b/",  "//h/a/",    "b/"],
      #["//h/a/b",   "//i/a/",    "//h/a/b"]];

define locators function-test relative-locator ()
  local method test-relative-locator
	    (class :: subclass(<locator>), info :: <vector>) => ()
	  let file1 = info[0];
	  let file2 = info[1];
	  let file3 = info[2];
	  check-equal(format-to-string("%s: relative(%=, %=) = %=",
				       class, file1, file2, file3),
		      relative-locator(as(class, file1),
				       as(class, file2)),
		      as(class, file3))
	end method test-relative-locator;
  for (class in vector(<microsoft-file-system-locator>, <posix-file-system-locator>))
    do(curry(test-relative-locator, class),
       $relative-tests)
  end;
  do(curry(test-relative-locator, <microsoft-file-system-locator>),
     $microsoft-relative-tests)
end function-test relative-locator;

define locators function-test merge-locators ()
  local method test-relative-locator
	    (class :: subclass(<locator>), info :: <vector>) => ()
	  let file1 = info[2];
	  let file2 = info[1];
	  let file3 = info[0];
	  check-equal(format-to-string("%s: merge(%=, %=) = %=",
				       class, file1, file2, file3),
		      merge-locators(as(class, file1),
				     as(class, file2)),
		      as(class, file3))
	end method test-relative-locator;
  for (class in vector(<microsoft-file-system-locator>, <posix-file-system-locator>))
    do(curry(test-relative-locator, class),
       $relative-tests)
  end;
  do(curry(test-relative-locator, <microsoft-file-system-locator>),
     $microsoft-relative-tests)
end function-test merge-locators;
