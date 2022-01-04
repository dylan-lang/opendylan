Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Locators class test cases

define test test-<server-locator> ()
  // Nothing to test
end test;

define test test-<physical-locator> ()
  // Nothing to test
end test;

define test test-<directory-locator> ()
  let locator = as(<directory-locator>, "home");
  assert-instance?(<directory-locator>, locator);
end test;

define test test-<file-locator> ()
  let locator = as(<file-locator>, "readme.txt");
  assert-instance?(<file-locator>, locator);
end test;

define test test-<native-directory-locator> ()
  let locator = as(<directory-locator>, "home");
  assert-instance?(<native-directory-locator>, locator);
end test;

define test test-<native-file-locator> ()
  let locator = as(<file-locator>, "readme.txt");
  assert-instance?(<native-file-locator>, locator);
end test;

define test test-<locator-error> ()
  assert-signals(<locator-error>, locator-error("test %s", "error"));
end test;


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

define test test-<url> ()
  for (url :: <string> in $valid-urls)
    check-equal(format-to-string("as(<string>, as(<url>, x)) = x [with x = %=]", url),
                as(<string>, as(<url>, url)),
                url)
  end
end test;

define test test-<web-locator> ()
  let web-locators = concatenate($valid-urls, #["mailto:andrewa@functionalobjects.com"]);
  for (name :: <string> in web-locators)
    check-equal(format-to-string("as(<string>, as(<web-loctor>, x)) = x [with x = %=]", name),
                as(<string>, as(<web-locator>, name)),
                name)
  end
end test;

define constant $valid-server-urls
  = #["http://www.functionalobjects.com",
      "http://www.functionalobjects.com:2000",
      "https://www.functionalobjects.com",
      "ftp://www.hello-world.org"];

define constant $invalid-server-urls
  = #["http://www.functionalobjects.com/path/to/resource", //  also has a path
      "file://c:", // interpreted as server c and no port
      "file:///c:"]; // a local file and not a server

define test test-<server-url> ()
  for (url :: <string> in $valid-server-urls)
    check-equal(format-to-string("as(<string>, as(<server-url>, x)) = x [with x = %=]", url),
                as(<string>, as(<server-url>, url)),
                url)
  end;
  for (url :: <string> in $invalid-server-urls)
    assert-signals(<locator-error>, as(<server-url>, url));
  end;
end test;

define test test-<http-server> ()
  let server = #f;
  check-equal("make <http-server>",
              server
                := make(<http-server>,
                        host: "www.fun.com",
                        port: 80,
                        username: "andrewa",
                        password: "test"),
              as(<http-server>, "http://www.fun.com:80?andrewa:test"));
end test;

define test test-<https-server> ()
  let server = #f;
  check-equal("make <http-server>",
              server
                := make(<https-server>,
                        host: "www.fun.com",
                        port: 80,
                        username: "andrewa",
                        password: "test"),
              as(<https-server>, "https://www.fun.com:80?andrewa:test"));
end test;

define test test-<ftp-server> ()
  assert-equal(make(<ftp-server>,
                    host: "ftp.fun.com",
                    port: 21),
               as(<ftp-server>, "ftp://ftp.fun.com:21"));
  assert-equal(make(<ftp-server>,
                    host: "ftp.fun.com",
                    port: 2100,
                    username: "andrewa",
                    password: "test"),
               as(<ftp-server>, "ftp://ftp.fun.com:2100?andrewa:test"));
end test;

define test test-<file-server> ()
  assert-equal(make(<file-server>,
                    host: "fun"),
               as(<file-server>, "file://fun"));
  assert-equal(make(<file-server>,
                    host: "fun",
                    port: 1234,
                    username: "andrewa",
                    password: "test"),
               as(<file-server>, "file://fun:1234?andrewa:test"));
end test;

define test test-<directory-url> ()
  let server = make(<http-server>,
                    host: "www.fun.com",
                    protocol: "http");
  assert-equal("http://www.fun.com/path/to/resource/",
               locator-as-string(<string>,
                                 make(<directory-url>,
                                      server: server,
                                      path: #["path", "to", "resource"])));
end test;

define test test-<file-url> ()
  let server = make(<http-server>,
                    host: "www.fun.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path", "to", "resource"]);
  assert-equal("http://www.fun.com/path/to/resource/info.txt",
               locator-as-string(<string>,
                                 make(<file-url>,
                                      directory: directory,
                                      base: "info",
                                      extension: "txt")));
end test;

define test test-<file-index-url> ()
  let server = make(<http-server>,
                    host: "www.fun.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path", "to", "resource"]);
  let file-url = make(<file-url>,
                      directory: directory,
                      base: "info",
                      extension: "txt");
  assert-equal("http://www.fun.com/path/to/resource/info.txt#toplevel",
               locator-as-string(<string>,
                                 make(<file-index-url>,
                                      file: file-url,
                                      index: "toplevel")));
end test;

define test test-<cgi-url> ()
  let server = make(<http-server>,
                    host: "www.fun.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path", "to", "resource"]);
  let file-url = make(<file-url>,
                      directory: directory,
                      base: "info",
                      extension: "txt");
  assert-equal("http://www.fun.com/path/to/resource/info.txt?lang=en",
               locator-as-string(<string>,
                                 make(<cgi-url>,
                                      file: file-url,
                                      cgi-string: "lang=en")));
end test;

define test test-<mail-to-locator> ()
  assert-equal("mailto:andrewa@fun.com",
               locator-as-string(<string>,
                                 make(<mail-to-locator>,
                                      address: "andrewa@fun.com")));
end test;


/// Locator function test cases

define test test-locator-error ()
  assert-signals(<locator-error>,
                 locator-error("test error %d", 1));
end test;

define test test-supports-open-locator? ()
  let locator1 = as(<file-locator>, "/test.txt");
  assert-true(supports-open-locator?(locator1));
  let locator2 = as(<mail-to-locator>, "mailto:andrewa@fun.com");
  assert-false(supports-open-locator?(locator2));
end test;

define test test-open-locator ()
  // Fill this in.
end test;

define test test-supports-list-locator? ()
  let locator1 = as(<file-locator>, "/test.txt");
  assert-false(supports-list-locator?(locator1));
  let locator2 = as(<file-locator>, "/tmp");
  assert-false(supports-list-locator?(locator2));
  let locator3 = as(<mail-to-locator>, "mailto:andrewa@fun.com");
  assert-false(supports-list-locator?(locator3));
end test;

define test test-list-locator ()
  // Create some structure
  let root = test-temp-directory();
  let dir1 = create-directory(root, "dir1");
  let file1 = merge-locators(as(<file-locator>, "file1"), root);
  let file2 = merge-locators(as(<file-locator>, "file2"), dir1);
  // Just need to touch the files, no need to write anything
  close(open-locator(file1, direction: #"output"));
  close(open-locator(file2, direction: #"output"));
  // test it.
  let entries = list-locator(root);
  assert-equal(2, size(entries));
  let found-file = any?(method(x)
                            instance?(x, <file-locator>) & x
                        end, entries);
  let found-dir = any?(method(x)
                           instance?(x, <directory-locator>) & x
                       end, entries);
  assert-equal(locator-name(file1), found-file & locator-name(found-file));
  assert-equal(locator-name(dir1), found-dir & locator-name(found-dir));
end test;

define test test-locator-host ()
  let locator = make(<http-server>, host: "www.fun.com");
  assert-equal("www.fun.com", locator-host(locator));
end test;

define test test-locator-server ()
  let locator = as(<url>, "http://www.fun.com/en/index.html");
  let server = make(<http-server>, host: "www.fun.com");
  assert-equal(server, locator-server(locator));
end test;

define test test-locator-volume ()
  let locator = as(<microsoft-directory-locator>, "C:\\Windows\\System");
  let server = locator-server(locator);
  assert-equal("C", locator-volume(server));
end test;

define test test-locator-directory ()
  let locator = as(<file-locator>, "/etc/fstab");
  let directory = as(<directory-locator>, "/etc/");
  assert-equal(directory, locator-directory(locator));
end test;

define test test-locator-relative? ()
  assert-true(locator-relative?(as(<file-locator>, "test.txt")));
  assert-true(locator-relative?(as(<file-locator>, "en/test.txt")));
  assert-false(locator-relative?(as(<file-locator>, "/usr/local/en/test.txt")));
  assert-true(locator-relative?(as(<file-locator>, "../test.txt")));
end test;

define test test-locator-path ()
  let locator = as(<file-locator>, "/var/log/system/system.log");
  assert-equal(#["var", "log", "system"], locator-path(locator));
end test;

define test test-locator-base ()
  assert-equal("example", locator-base(as(<file-locator>, "example.txt")));
  assert-equal("example", locator-base(as(<file-locator>, "example")));
  assert-equal("example", locator-base(as(<file-locator>, "/home/andrewa/example.dylan")));
end test;

define test test-locator-extension ()
  assert-equal("txt", locator-extension(as(<file-locator>, "example.txt")));
  assert-equal(#f, locator-extension(as(<file-locator>, "example")));
  assert-equal("dylan", locator-extension(as(<file-locator>, "/home/andrewa/example.dylan")));
end test;

define test test-locator-name ()
  assert-equal("example.txt", locator-name(as(<file-locator>, "example.txt")));
  assert-equal("example", locator-name(as(<file-locator>, "example")));
  assert-equal("example.dylan", locator-name(as(<file-locator>, "/home/andrewa/example.dylan")));
  assert-equal("d", locator-name(as(<directory-locator>, "/a/b/c/d")));
  assert-equal(".", locator-name(as(<directory-locator>, ".")));
end test;

/// Coercion protocols

define test test-locator-as-string ()
  let server = make(<http-server>,
                    host: "www.fun.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path", "to", "resource"]);
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
                     address: "andrewa@fun.com");
  assert-equal("http://www.fun.com", locator-as-string(<string>, server));
  assert-equal("http://www.fun.com/path/to/resource/", locator-as-string(<string>, directory));
  assert-equal("http://www.fun.com/path/to/resource/info.txt", locator-as-string(<string>, file-url));
  assert-equal("http://www.fun.com/path/to/resource/info.txt?lang=en", locator-as-string(<string>, cgi-url));
  assert-equal("http://www.fun.com/path/to/resource/info.txt#toplevel", locator-as-string(<string>, file-index-url));
  assert-equal("mailto:andrewa@fun.com", locator-as-string(<string>, mail-to));
end test;

define test test-string-as-locator ()
  let server = make(<http-server>,
                    host: "www.fun.com",
                    protocol: "http");
  let directory = make(<directory-url>,
                       server: server,
                       path: #["path", "to", "resource"]);
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
                     address: "andrewa@fun.com");
  assert-equal(directory, string-as-locator(<url>, "http://www.fun.com/path/to/resource/"));
  assert-equal(file-url, string-as-locator(<url>, "http://www.fun.com/path/to/resource/info.txt"));
  assert-equal(cgi-url, string-as-locator(<url>, "http://www.fun.com/path/to/resource/info.txt?lang=en"));
  assert-equal(file-index-url, string-as-locator(<url>, "http://www.fun.com/path/to/resource/info.txt#toplevel"));
  assert-equal(mail-to, string-as-locator(<web-locator>, "mailto:andrewa@fun.com"));
end test;

/// Utilities

define constant $simplify-tests
  = #[#["a",         "a"],
      #["a.t",       "a.t"],
      #["./",        "./"],        // This one is confusing, but correct
      #["./a.t",     "a.t"],
      #["a/./b.t",   "a/b.t"],
      #["../a/b",    "../a/b"],
      #["../a/./b",  "../a/b"],
      #["a/b/../c", "a/c"]];

define constant $microsoft-simplify-tests
  = #[#["a/../b.t",  "b.t"],
      #["/a/../b.t", "/b.t"]];

define test test-simplify-locator ()
  local method check-simplify-locator
            (class :: subclass(<locator>), path1, path2) => ()
          check-equal(format-to-string("%s: simplify-locator(%=) = %=",
                                       class, path1, path2),
                      simplify-locator(as(class, path1)),
                      as(class, path2))
        end method;
  for (class in vector(<microsoft-file-system-locator>, <posix-file-system-locator>))
    for (info in $simplify-tests)
      apply(check-simplify-locator, class, info);
    end;
  end;
  for (info in $microsoft-simplify-tests)
    apply(check-simplify-locator, <microsoft-file-system-locator>, info)
  end;
end test;

define constant $subdirectory-tests
  = #[#["a/",    #["b"],      "a/b/"],
      #["a/b/",  #["c", "d"], "a/b/c/d/"]];

define constant $microsoft-subdirectory-tests
  = #[#["h:/a/",  #["b"],    "h:/a/b/"],
      #["h:/a/",  #["b"],    "H:/a/b/"],
      #["//h/a/", #["b"],    "//h/a/b/"]];

define test test-subdirectory-locator ()
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
end test;

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

define test test-relative-locator ()
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
end test;

define constant $posix-merge-tests
  //    orig      merge-from         expect
  = #[#["a",      "a",               "a"],
      #["a",      "b",               "a"],
      #["./",     "a/",              "a/./"],
      #["b",      "a/",              "a/b"],
      #["a/",     "b",               "a/b"],
      #["../b/c", "a/",              "a/../b/c"],
      #["c",      "/a/b/",           "/a/b/c"],
      #["b/c",    "/a/",             "/a/b/c"],
      #["/a/",    "b/c",             "/a/c"],
      #["../",    "/a/b/c/",         "/a/b/c/../"],
      #["../../../a/b/c", "/d/e/f/", "/d/e/f/../../../a/b/c"]];

define constant $microsoft-merge-tests
  //    orig      merge-from         expect
  = #[#["a",      "a",               "a"],
      #["a",      "b",               "a"],
      #["./",     "a/",              "a/./"],
      #["b",      "a/",              "a/b"],
      #["a/",     "b",               "a/b"],
      #["../b/c", "a/",              "a/../b/c"],
      #["c",      "/a/b/",           "/a/b/c"],
      #["b/c",    "/a/",             "/a/b/c"],
      #["../",    "/a/b/c/",         "/a/b/c/../"],
      #["../../../a/b/c", "/d/e/f/", "/d/e/f/../../../a/b/c"],
      #["c",      "h:/a/b/",         "h:/a/b/c"],
      #["c/",     "h:/a/b/",         "h:/a/b/c/"],
      #["h:/a/b/c", "i:/a/b/c/",     "h:/a/b/c"],
      #["h:/a/b/c/", "i:/a/b/c/",    "h:/a/b/c/"],
      #["b",      "//h/a/",          "//h/a/b"],
      #["b/",     "//h/a/",          "//h/a/b/"],
      #["//h/a/b", "//i/a/",         "//h/a/b"]];

// Note that merge-locators calls simplify-locator so this is effectively
// testing simplify-locator as well.
define test test-merge-locators ()
  local method check-merge-locator
            (class :: subclass(<locator>), path1, path2, path3)
          check-equal(format-to-string("%s: merge-locators(%=, %=) = %=",
                                       class, path1, path2, path3),
                      merge-locators(as(class, path1),
                                     as(class, path2)),
                      as(class, path3));
        end method;
  for (paths in $posix-merge-tests)
    apply(check-merge-locator, <posix-file-system-locator>, paths)
  end;
  for (paths in $microsoft-merge-tests)
    apply(check-merge-locator, <microsoft-file-system-locator>, paths)
  end;
end test;

// TODO(cgay): create a link and verify that resolve-locator respects the
// link. Currently there is no API to create a link.
define test test-resolve-locator ()
  let tmpdir = test-temp-directory();
  assert-signals(<file-system-error>,
                 resolve-locator(subdirectory-locator(tmpdir, "non-existent")));

  create-directory(tmpdir, "foo");
  create-directory(tmpdir, "bar");
  let foo = subdirectory-locator(tmpdir, "foo");
  let bar = subdirectory-locator(tmpdir, "bar");
  let foob = subdirectory-locator(foo, "b");
  create-directory(foo, "b");
  let pname = as(<string>, bar);
  assert-equal(as(<string>, resolve-locator(bar)), pname);
  for (item in list(list(#["foo"], foo),
                    list(#["bar"], bar),
                    list(#["foo", "..", "bar"], bar),
                    list(#["foo", ".."], tmpdir),
                    list(#["foo", ".", "b", "..", "..", "foo"], foo)))
    let (subdirs, want) = apply(values, item);
    let orig = apply(subdirectory-locator, tmpdir, subdirs);
    let got = resolve-locator(orig);
    assert-equal(got, want, format-to-string("resolve-locator(%=) => %=", orig, got));
  end;
end test;

// non-file-system-locators-test-suite?
define suite more-locators-test-suite ()
  test test-<server-locator>;
  test test-<physical-locator>;
  test test-<directory-locator>;
  test test-<file-locator>;
  test test-<native-directory-locator>;
  test test-<native-file-locator>;
  test test-<locator-error>;
  test test-<url>;
  test test-<web-locator>;
  test test-<server-url>;
  test test-<http-server>;
  test test-<https-server>;
  test test-<ftp-server>;
  test test-<file-server>;
  test test-<directory-url>;
  test test-<file-url>;
  test test-<file-index-url>;
  test test-<cgi-url>;
  test test-<mail-to-locator>;
  test test-locator-error;
  test test-supports-open-locator?;
  test test-open-locator;
  test test-supports-list-locator?;
  test test-list-locator;
  test test-locator-host;
  test test-locator-server;
  test test-locator-volume;
  test test-locator-directory;
  test test-locator-relative?;
  test test-locator-path;
  test test-locator-base;
  test test-locator-extension;
  test test-locator-name;
  test test-locator-as-string;
  test test-string-as-locator;
  test test-simplify-locator;
  test test-subdirectory-locator;
  test test-relative-locator;
  test test-merge-locators;
  test test-resolve-locator;
end suite;
