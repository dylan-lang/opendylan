Module:       system-test-suite
Synopsis:     System library test suite
Author:              Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Locators class test cases

define locators class-test <locator> ()
  // Nothing to test
end class-test <locator>;

define locators class-test <server-locator> ()
  // Nothing to test
end class-test <server-locator>;

define locators class-test <physical-locator> ()
  // Nothing to test
end class-test <physical-locator>;

define locators class-test <directory-locator> ()
  let locator = as(<directory-locator>, "home");
  assert-instance?(<directory-locator>, locator);
end class-test <directory-locator>;

define locators class-test <file-locator> ()
  let locator = as(<file-locator>, "readme.txt");
  assert-instance?(<file-locator>, locator);
end class-test <file-locator>;

define locators class-test <native-directory-locator> ()
  let locator = as(<directory-locator>, "home");
  assert-instance?(<native-directory-locator>, locator);
end class-test <native-directory-locator>;

define locators class-test <native-file-locator> ()
  let locator = as(<file-locator>, "readme.txt");
  assert-instance?(<native-file-locator>, locator);
end class-test <native-file-locator>;

define locators class-test <locator-error> ()
  assert-signals(<locator-error>, locator-error("test %s", "error"));
end class-test <locator-error>;


/// Web locator classes

define sideways method make-test-instance
    (class :: subclass(<server-url>))
 => (instance :: <server-url>)
  make(class, host: "www.functionalobjects.com")
end method make-test-instance;

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
      "ftp://hello-world.org?andrewa:my-password/pub"];

define locators class-test <url> ()
  for (url :: <string> in $valid-urls)
    check-equal(format-to-string("as(<string>, as(<url>, x)) = x [with x = %=]", url),
                as(<string>, as(<url>, url)),
                url)
  end
end class-test <url>;

define locators class-test <web-locator> ()
  let web-locators = concatenate($valid-urls, #["mailto:andrewa@functionalobjects.com"]);
  for (name :: <string> in web-locators)
    check-equal(format-to-string("as(<string>, as(<web-loctor>, x)) = x [with x = %=]", name),
                as(<string>, as(<web-locator>, name)),
                name)
  end
end class-test <web-locator>;

define constant $valid-server-urls
  = #["http://www.functionalobjects.com",
      "http://www.functionalobjects.com:2000",
      "https://www.functionalobjects.com",
      "ftp://www.hello-world.org"];

define constant $invalid-server-urls
  = #["http://www.functionalobjects.com/path/to/resource", //  also has a path
      "file://c:", // interpreted as server c and no port
      "file:///c:"]; // a local file and not a server

define locators class-test <server-url> ()
  for (url :: <string> in $valid-server-urls)
    check-equal(format-to-string("as(<string>, as(<server-url>, x)) = x [with x = %=]", url),
                as(<string>, as(<server-url>, url)),
                url)
  end;
  for (url :: <string> in $invalid-server-urls)
    assert-signals(<locator-error>, as(<server-url>, url));
  end;
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
  assert-equal(make(<ftp-server>,
                    host: "ftp.functionalobjects.com",
                    port: 21),
               as(<ftp-server>, "ftp://ftp.functionalobjects.com:21"));
  assert-equal(make(<ftp-server>,
                    host: "ftp.functionalobjects.com",
                    port: 2100,
                    username: "andrewa",
                    password: "test"),
               as(<ftp-server>, "ftp://ftp.functionalobjects.com:2100?andrewa:test"));
end class-test <ftp-server>;

define locators class-test <file-server> ()
  assert-equal(make(<file-server>,
                    host: "functionalobjects"),
               as(<file-server>, "file://functionalobjects"));
  assert-equal(make(<file-server>,
                    host: "functionalobjects",
                    port: 1234,
                    username: "andrewa",
                    password: "test"),
               as(<file-server>, "file://functionalobjects:1234?andrewa:test"));
end class-test <file-server>;

define locators class-test <directory-url> ()
  let server = make(<http-server>,
                    host: "www.functionalobjects.com",
                    protocol: "http");
  assert-equal("http://www.functionalobjects.com/path/to/resource/",
               locator-as-string(<string>,
                                 make(<directory-url>,
                                      server: server,
                                      path: #["path","to","resource"])));
end class-test <directory-url>;

define locators class-test <file-url> ()
  let server = make(<http-server>,
                    host: "www.functionalobjects.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path","to","resource"]);
  assert-equal("http://www.functionalobjects.com/path/to/resource/info.txt",
               locator-as-string(<string>,
                                 make(<file-url>,
                                      directory: directory,
                                      base: "info",
                                      extension: "txt")));
end class-test <file-url>;

define locators class-test <file-index-url> ()
  let server = make(<http-server>,
                    host: "www.functionalobjects.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path","to","resource"]);
  let file-url = make(<file-url>,
                      directory: directory,
                      base: "info",
                      extension: "txt");
  assert-equal("http://www.functionalobjects.com/path/to/resource/info.txt#toplevel",
               locator-as-string(<string>,
                                 make(<file-index-url>,
                                      file: file-url,
                                      index: "toplevel")));
end class-test <file-index-url>;

 define locators class-test <cgi-url> ()
  let server = make(<http-server>,
                    host: "www.functionalobjects.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path","to","resource"]);
  let file-url = make(<file-url>,
                      directory: directory,
                      base: "info",
                      extension: "txt");
  assert-equal("http://www.functionalobjects.com/path/to/resource/info.txt?lang=en",
               locator-as-string(<string>,
                                 make(<cgi-url>,
                                      file: file-url,
                                      cgi-string: "lang=en")));
end class-test <cgi-url>;

define locators class-test <mail-to-locator> ()
  assert-equal("mailto:andrewa@functionalobjects.com",
               locator-as-string(<string>,
                                 make(<mail-to-locator>,
                                      address: "andrewa@functionalobjects.com")));
end class-test <mail-to-locator>;


/// Locator function test cases

define locators function-test locator-error ()
  assert-signals(<locator-error>,
                 locator-error("test error %d", 1));
end function-test locator-error;

define locators function-test supports-open-locator? ()
  let locator1 = as(<file-locator>, "/test.txt");
  assert-true(supports-open-locator?(locator1));
  let locator2 = as(<mail-to-locator>, "mailto:andrewa@functionalobjects.com");
  assert-false(supports-open-locator?(locator2));
end function-test supports-open-locator?;

define locators function-test open-locator ()
  // Unable to test this
end function-test open-locator;

define locators function-test supports-list-locator? ()
  let locator1 = as(<file-locator>, "/test.txt");
  assert-false(supports-list-locator?(locator1));
  let locator2 = as(<file-locator>, "/tmp");
  assert-false(supports-list-locator?(locator2));
  let locator3 = as(<mail-to-locator>, "mailto:andrewa@functionalobjects.com");
  assert-false(supports-list-locator?(locator3));
end function-test supports-list-locator?;

define locators function-test list-locator ()
  // Unable to test this
end function-test list-locator;

define locators function-test locator-host ()
  let locator = make(<http-server>, host: "www.functionalobjects.com");
  assert-equal("www.functionalobjects.com", locator-host(locator));
end function-test locator-host;

define locators function-test locator-server ()
  let locator = as(<url>, "http://www.functionalobjects.com/en/index.html");
  let server = make(<http-server>, host: "www.functionalobjects.com");
  assert-equal(server, locator-server(locator));
end function-test locator-server;

define locators function-test locator-volume ()
  let locator = as(<microsoft-directory-locator>, "C:\\Windows\\System");
  let server = locator-server(locator);
  assert-equal("C", locator-volume(server));
end function-test locator-volume;

define locators function-test locator-directory ()
  let locator = as(<file-locator>, "/etc/fstab");
  let directory = as(<directory-locator>, "/etc/");
  assert-equal(directory, locator-directory(locator));
end function-test locator-directory;

define locators function-test locator-relative? ()
  assert-true(locator-relative?(as(<file-locator>, "test.txt")));
  assert-true(locator-relative?(as(<file-locator>, "en/test.txt")));
  assert-false(locator-relative?(as(<file-locator>, "/usr/local/en/test.txt")));
  assert-true(locator-relative?(as(<file-locator>, "../test.txt")));
end function-test locator-relative?;

define locators function-test locator-path ()
  let locator = as(<file-locator>, "/var/log/system/system.log");
  assert-equal(#["var", "log", "system"], locator-path(locator));
end function-test locator-path;

define locators function-test locator-base ()
  assert-equal("example", locator-base(as(<file-locator>, "example.txt")));
  assert-equal("example", locator-base(as(<file-locator>, "example")));
  assert-equal("example", locator-base(as(<file-locator>, "/home/andrewa/example.dylan")));
end function-test locator-base;

define locators function-test locator-extension ()
  assert-equal("txt", locator-extension(as(<file-locator>, "example.txt")));
  assert-equal(#f, locator-extension(as(<file-locator>, "example")));
  assert-equal("dylan", locator-extension(as(<file-locator>, "/home/andrewa/example.dylan")));
end function-test locator-extension;

define locators function-test locator-name ()
  assert-equal("example.txt", locator-name(as(<file-locator>, "example.txt")));
  assert-equal("example", locator-name(as(<file-locator>, "example")));
  assert-equal("example.dylan", locator-name(as(<file-locator>, "/home/andrewa/example.dylan")));
end function-test locator-name;

/// Coercion protocols

define locators function-test locator-as-string ()
  let server = make(<http-server>,
                    host: "www.functionalobjects.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path","to","resource"]);
  let file-url = make(<file-url>,
                      directory: directory,
                      base: "info",
                      extension: "txt");
  let cgi-url = make(<cgi-url>,
                     file: file-url,
                     cgi-string: "lang=en");
  let file-index-url = make(<file-index-url>,
                            file: file-url,
                            index: "toplevel");
  let mail-to = make(<mail-to-locator>,
                     address: "andrewa@functionalobjects.com");
  assert-equal("http://www.functionalobjects.com", locator-as-string(<string>, server));
  assert-equal("http://www.functionalobjects.com/path/to/resource/", locator-as-string(<string>, directory));
  assert-equal("http://www.functionalobjects.com/path/to/resource/info.txt", locator-as-string(<string>, file-url));
  assert-equal("http://www.functionalobjects.com/path/to/resource/info.txt?lang=en", locator-as-string(<string>, cgi-url));
  assert-equal("http://www.functionalobjects.com/path/to/resource/info.txt#toplevel", locator-as-string(<string>, file-index-url));
  assert-equal("mailto:andrewa@functionalobjects.com", locator-as-string(<string>, mail-to));
end function-test locator-as-string;

define locators function-test string-as-locator ()
let server = make(<http-server>,
                    host: "www.functionalobjects.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path","to","resource"]);
  let file-url = make(<file-url>,
                      directory: directory,
                      base: "info",
                      extension: "txt");
  let cgi-url = make(<cgi-url>,
                     file: file-url,
                     cgi-string: "lang=en");
  let file-index-url = make(<file-index-url>,
                            file: file-url,
                            index: "toplevel");
  let mail-to = make(<mail-to-locator>,
                     address: "andrewa@functionalobjects.com");
  assert-equal(directory, string-as-locator(<url>, "http://www.functionalobjects.com/path/to/resource/"));
  assert-equal(file-url, string-as-locator(<url>, "http://www.functionalobjects.com/path/to/resource/info.txt"));
  assert-equal(cgi-url, string-as-locator(<url>, "http://www.functionalobjects.com/path/to/resource/info.txt?lang=en"));
  assert-equal(file-index-url, string-as-locator(<url>, "http://www.functionalobjects.com/path/to/resource/info.txt#toplevel"));
  assert-equal(mail-to, string-as-locator(<web-locator>, "mailto:andrewa@functionalobjects.com"));
end function-test string-as-locator;

/// Utilities

define constant $simplify-tests
  = #[#["a",         "a"],
      #["a.t",       "a.t"],
      #["./",        "./"],        // This one is confusing, but correct
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

define constant $posix-merge-tests
  = #[#["a",      "a",               "a"],
      #["b",      "a",               "a"],
      #["a/",     "./",              "a/"],
      #["a/",     "b",               "a/b"],
      #["a/",     "b",               "a/b"],
      #["a/",     "../b/c",          "a/../b/c"],
      #["/a/b/",  "c",               "/a/b/c"],
      #["/a/",    "b/c",             "/a/b/c"],
      #["/a/b/c/", "../",            "/a/b/c/../"],
      #["/d/e/f/", "../../../a/b/c", "/d/e/f/../../../a/b/c"]];

define constant $microsoft-merge-tests
  = #[#["a",      "a",               "a"],
      #["b",      "a",               "a"],
      #["a/",     "./",              "a/"],
      #["a/",     "b",               "a/b"],
      #["a/",     "b",               "a/b"],
      #["a/",     "../b/c",          "b/c"],
      #["/a/b/",  "c",               "/a/b/c"],
      #["/a/",    "b/c",             "/a/b/c"],
      #["/a/b/c/", "../",            "/a/b/"],
      #["/d/e/f/", "../../../a/b/c", "/a/b/c"],
      #["h:/a/b/",   "c",         "h:/a/b/c"],
      #["h:/a/b/",   "c/",        "h:/a/b/c/"],
      #["i:/a/b/c/", "h:/a/b/c",  "h:/a/b/c"],
      #["i:/a/b/c/", "h:/a/b/c/", "h:/a/b/c/"],
      #["//h/a/",    "b",         "//h/a/b"],
      #["//h/a/",    "b/",        "//h/a/b/"],
      #["//i/a/",    "//h/a/b",   "//h/a/b"]];

define locators function-test merge-locators ()
  local method test-merge-locator
          (class :: subclass(<locator>), info :: <vector>) => ()
          let file1 = info[1];
          let file2 = info[0];
          let file3 = info[2];
          check-equal(format-to-string("%s: merge(%=, %=) = %=",
                                       class, file1, file2, file3),
                      merge-locators(as(class, file1),
                                     as(class, file2)),
                      as(class, file3));
        end method test-merge-locator;
  do(curry(test-merge-locator, <posix-file-system-locator>),
     $posix-merge-tests);
  do(curry(test-merge-locator, <microsoft-file-system-locator>),
     $microsoft-merge-tests)
end function-test merge-locators;
