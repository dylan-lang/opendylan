Module:    locators-test-suite
Author:    Tim McNerney
Synopsis:  more tests for the locators library
Revision:  September 20 1996 by amit. TW+ & Webster compatible
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test AS-DIRECTORY-LOCATOR-TESTS (title: "as <directory-locator> tests")
  check-true("as <directory-locator> tests", 
  as(<string>, as(<directory-locator>, as(<locator>, "/foo/dir"))) = "/foo/dir/");
end test AS-DIRECTORY-LOCATOR-TESTS;


define test AS-FILE-LOCATOR-TESTS (title: "as <file-locator> tests")
  check-true("as <file-locator> tests", 
  as(<string>, as(<file-locator>, as(<locator>, "/foo/dir/"))) = "/foo/dir");
end test AS-FILE-LOCATOR-TESTS;

define test ROMANS-BUG (title: "Roman's bug")
  check-true("Roman's bug", make(<abstract-locator>, base: "dylan", extension: "ei"));
end test ROMANS-BUG;

define test ABBREVIATE-TESTS (title: "Abbreviate tests")
  /*
  let old-default = <native-locator>;
  block ()
    <native-locator> := <microsoft-locator>;

       check-true("abbreviate-locator test #1", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\foo.txt", "\\a\\b\\c\\d\\"))
	 = "..\\foo.txt");
       check-true("abbreviate-locator test #2", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\foo.txt", "\\a\\x\\c\\d\\"))
	 = "\\a\\b\\c\\foo.txt");
       check-true("abbreviate-locator test #3", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\foo.txt", "\\a\\b\\x\\"))
	 = "..\\c\\foo.txt");
       check-true("abbreviate-locator test #4", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\foo.txt", "\\a\\b\\"))
	 = "c\\foo.txt");
       check-true("abbreviate-locator test #5", 
       as(<string>, abbreviate-locator("\\a\\b\\foo.txt", "\\a\\b\\c\\d\\"))
	 = "\\a\\b\\foo.txt");
       check-true("abbreviate-locator test #6", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\d\\foo.txt", "\\a\\b\\"))
	 = "c\\d\\foo.txt");
       check-true("abbreviate-locator test #7", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\d\\foo.txt", ""))
	 = "\\a\\b\\c\\d\\foo.txt");
       check-true("abbreviate-locator test #8", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\d\\foo.txt", 
				       override-locator(as(<locator>, ""),
							type: #(#"text"))))
	 = "\\a\\b\\c\\d\\foo");
       check-true("abbreviate-locator test #9", 
       as(<string>, abbreviate-locator("\\a\\b\\c\\d\\foo.txt", 
				       override-locator(as(<locator>, ""),
							extension: ".txt")))
	 = "\\a\\b\\c\\d\\foo");
       check-true("abbreviate-locator test #10", 
       as(<string>, abbreviate-locator("a\\b\\c\\d\\foo.txt", "a\\b\\"))
	 = "c\\d\\foo.txt");
  cleanup
    <native-locator> := old-default
  end block;
  */
end test ABBREVIATE-TESTS;

define method MERGE-ABBREV-TEST (x, d)
  let a = abbreviate-locator (x, d);
  merge-locators(a, d) = as(<locator>, x)
end method;

define test ABBREVIATE-MERGE-TESTS (title: "merge-locators(abbreviate-locator(x, d), d) = x tests")
     check-true("merge-abbrev-test #1", 
  merge-abbrev-test("abstract://host/a/b/c/foo.text", "abstract://host/a/b/c/d/"));
   check-true("merge-abbrev-test #2", 
  merge-abbrev-test("abstract://host/a/b/c/foo.text", "abstract://host/a/x/c/d/"));
   check-true("merge-abbrev-test #3", 
  merge-abbrev-test("abstract://host/a/b/c/foo.text", "abstract://host/a/b/x/"));
   check-true("merge-abbrev-test #4", 
  merge-abbrev-test("abstract://host/a/b/c/foo.text", "abstract://host/a/b/"));
   check-true("merge-abbrev-test #5", 
  merge-abbrev-test("abstract://host/a/b/foo.text", "abstract://host/a/b/c/d/"));
   check-true("merge-abbrev-test #6", 
  merge-abbrev-test("abstract://host/a/b/c/d/foo.text", "abstract://host/a/b/"));
   check-true("merge-abbrev-test #7", 
  merge-abbrev-test("abstract://host/a/b/c/d/foo.text", ""));
   check-true("merge-abbrev-test #8", 
  merge-abbrev-test("abstract://host/a/b/c/d/foo.text", 
                    override-locator(as(<abstract-locator>, ""), type: #(#"text"))));
   check-true("merge-abbrev-test #9", 
  merge-abbrev-test("abstract://host/a/b/c/d/foo.text", 
                    override-locator(as(<abstract-locator>, ""), extension: ".text")));
   check-true("merge-abbrev-test #10", 
  merge-abbrev-test(as(<abstract-locator>, "a/b/c/d/foo.text"),
		    as(<abstract-locator>, "a/b/")));
end test ABBREVIATE-MERGE-TESTS;


// 3/27/95 - All but the first test failed until I implemented delayed name
//           derivation for <http-locator>.  This shouldn't have happened.
//           Need to change base: and type: to #"derived" and add name:
//           where appropriate
define test HTTP-LOCATOR-TESTS (title: "<http-locator> tests")
     check-true("<http-locator> test #1", 
  as(<locator>, "http://mumble.com")
    = make(<http-directory-locator>, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: #f, base: #f, type: #f, search-keys: #f));
   check-true("<http-locator> test #2", 
  as(<locator>, "http://mumble.com?index")
    = make(<http-directory-locator>, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: #f, base: #f, type: #f, search-keys: "index"));
   check-true("<http-locator> test #3", 
  as(<locator>, "http://mumble.com/zap.html")
    = make(<http-file-locator>, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"html")), search-keys: #f));
   check-true("<http-locator> test #4", 
  as(<locator>, "http://mumble.com:5001/zap.html")
    = make(<http-file-locator>, host: make(<host-path>, elements: #("mumble", "com")), port: 5001, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"html")), search-keys: #f));
   check-true("<http-locator> test #5", 
  as(<locator>, "http://mumble.com/zap.html?heading")
    = make(<http-file-locator>, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"html")), search-keys: "heading"));
   check-true("<http-locator> test #6", 
  as(<locator>, "http://mumble.com")
    = make(<http-locator>, prefix: "http://mumble.com"));
   check-true("<http-locator> test #7", 
  as(<locator>, "http://mumble.com?index")
    = make(<http-locator>, prefix: "http://mumble.com", search-keys: "index"));
   check-true("<http-locator> test #8", 
  as(<locator>, "http://mumble.com/zap.html")
    = make(<http-locator>, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", extension: ".html"));
   check-true("<http-locator> test #9", 
  as(<locator>, "http://mumble.com:5001/zap.html")
    = make(<http-locator>, prefix: "//mumble.com:5001/", name: "zap.html"));
   check-true("<http-locator> test #10", 
  as(<locator>, "http://mumble.com/zap.html?heading")
    = make(<http-locator>, prefix: "//mumble.com/", name: "zap.html", search-keys: "heading"));
end test HTTP-LOCATOR-TESTS;


define test FTP-LOCATOR-TESTS (title: "<ftp-locator> tests")
     check-true("<ftp-locator> test #1", 
  as(<locator>, "ftp://mumble.com") // This isn't strictly legal (although the HTTP version is)
    = make(<ftp-directory-locator>, user-id: #f, password: #f, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: #f, base: #f, type: #f, transfer-type: #f));
   check-true("<ftp-locator> test #2", 
  as(<locator>, "ftp://mumble.com/zap.text")
    = make(<ftp-file-locator>, user-id: #f, password: #f, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"text")), transfer-type: #f));
   check-true("<ftp-locator> test #3", 
  as(<locator>, "ftp://mumble.com:5001/zap.text")
    = make(<ftp-file-locator>, user-id: #f, password: #f, host: make(<host-path>, elements: #("mumble", "com")), port: 5001, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"text")), transfer-type: #f));
   check-true("<ftp-locator> test #4", 
  as(<locator>, "ftp://tim@mumble.com/zap.text")
    = make(<ftp-file-locator>, user-id: "tim", password: #f, host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"text")), transfer-type: #f));
   check-true("<ftp-locator> test #5", 
  as(<locator>, "ftp://tim:sesame@mumble.com/zap.text")
    = make(<ftp-file-locator>, user-id: "tim", password: "sesame", host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"text")), transfer-type: #f));
   check-true("<ftp-locator> test #6", 
  as(<locator>, "ftp://tim:sesame@mumble.com:5001/zap.text")
    = make(<ftp-file-locator>, user-id: "tim", password: "sesame", host: make(<host-path>, elements: #("mumble", "com")), port: 5001, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"text")), transfer-type: #f));
   check-true("<ftp-locator> test #7", 
  as(<ftp-transfer-type>, ";type=L16")
    = make(<ftp-transfer-type>, type-code: 'L', form-code: #f, byte-size: 16));
   check-true("<ftp-locator> test #8", 
  as(<ftp-transfer-type>, ";type=AN")
    = make(<ftp-transfer-type>, type-code: 'A', form-code: 'N', byte-size: #f));
   check-true("<ftp-locator> test #9", 
  as(<ftp-transfer-type>, ";type=A")
    = make(<ftp-transfer-type>, type-code: 'A', form-code: 'N', byte-size: #f));
   check-true("<ftp-locator> test #10", 
  as(<string>,as(<ftp-transfer-type>, ";type=A"))
    = ";type=AN");
   check-true("<ftp-locator> test #11", 
  as(<locator>, "ftp://tim:sesame@mumble.com/zap.text;type=EC")
    = make(<ftp-file-locator>, user-id: "tim", password: "sesame", host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #(#"text")), transfer-type: make(<ftp-transfer-type>, type-code: 'E', form-code: 'C', byte-size: #f)));
   check-true("<ftp-locator> test #12", 
   as(<locator>, "ftp://tim:sesame@mumble.com/zap.bin;type=L8")
     = make(<ftp-file-locator>, user-id: "tim", password: "sesame", host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), base: "zap", type: make(<posix-type-path>, elements: #("bin")), transfer-type: make(<ftp-transfer-type>, type-code: 'L', form-code: #f, byte-size: 8)));
   check-true("<ftp-locator> test #13", 
  as(<locator>, "ftp://mumble.com") // This isn't strictly legal (although the HTTP version is)
    = make(<ftp-locator>, prefix: "//mumble.com"));
   check-true("<ftp-locator> test #14", 
  as(<locator>, "ftp://mumble.com/zap.text")
    = make(<ftp-locator>, host: make(<host-path>, elements: #("mumble", "com")), directory: make(<abstract-directory-path>, elements: #()), base: "zap", extension: ".text"));
   check-true("<ftp-locator> test #15", 
  as(<locator>, "ftp://mumble.com:5001/zap.text")
    = make(<ftp-locator>, prefix: "ftp://mumble.com:5001/", name: "zap.text"));
   check-true("<ftp-locator> test #16", 
  as(<locator>, "ftp://tim@mumble.com/zap.text")
    = make(<ftp-locator>, prefix: "ftp://tim@mumble.com/", name: "zap.text"));
   check-true("<ftp-locator> test #17", 
  as(<locator>, "ftp://tim:sesame@mumble.com/zap.text")
    = make(<ftp-locator>, prefix: "//tim:sesame@mumble.com/", name: "zap.text"));
   check-true("<ftp-locator> test #18", 
  as(<locator>, "ftp://tim:sesame@mumble.com:5001/zap.text")
    = make(<ftp-locator>, prefix: "ftp://tim:sesame@mumble.com:5001/", name: "zap.text"));
   check-true("<ftp-locator> test #19", 
  as(<locator>, "ftp://tim:sesame@mumble.com/zap.text;type=EC")
    = make(<ftp-locator>, prefix: "//tim:sesame@mumble.com/", base: "zap", type: make(<posix-type-path>, elements: #(#"text")), transfer-type: make(<ftp-transfer-type>, type-code: 'E', form-code: 'C', byte-size: #f)));
   check-true("<ftp-locator> test #20", 
  as(<locator>, "ftp://tim:sesame@mumble.com/zap.bin;type=L8")
    = make(<ftp-locator>, user-id: "tim", password: "sesame", host: make(<host-path>, elements: #("mumble", "com")), port: #f, directory: make(<abstract-directory-path>, elements: #()), name: "zap.bin", transfer-type: make(<ftp-transfer-type>, type-code: 'L', form-code: #f, byte-size: 8)));
end test FTP-LOCATOR-TESTS;


define test MERGE-TESTS (title: "Merge tests")
     check-true("merge locators test #1", 
  merge-locators("abstract://foo/bar/", as(<abstract-locator>, "mumble.dylan"))
    = as(<locator>, "abstract://foo/bar/mumble.dylan"));
   check-true("merge locators test #2", 
  as(<string>,merge-locators("bar.Z", "/foo/")) = "/foo/bar.Z"  );
   check-true("merge locators test #3", 
  as(<string>,merge-locators("baz/bar.Z", "/foo/")) = "/foo/baz/bar.Z");
end test MERGE-TESTS;


define test MICROSOFT-LOCATOR-TESTS (title: "<microsoft-locator> tests")
     check-true("<microsoft-locator> test #1", 
  as(<microsoft-locator>, "\\\\mumble\\foo\\bar\\bax.txt")
    = make(<unc-file-locator>, host: make(<microsoft-host-path>, elements: #("mumble")), directory: make(<microsoft-directory-path>, elements: #("foo", "bar")), base: "bax", type: make(<microsoft-type-path>, elements: #(#"text"))));
   check-true("<microsoft-locator> test #2", 
  as(<string>, as(<microsoft-locator>, "\\\\mumble\\foo\\bar\\bax.txt"))
    = "\\\\mumble\\foo\\bar\\bax.txt");
   check-true("<microsoft-locator> test #3", 
  as(<microsoft-locator>, "e:\\foo\\bar\\bax.txt")
    = make(<ms-dos-file-locator>, volume: "e", directory: make(<microsoft-directory-path>, elements: #("foo", "bar")), base: "bax", type: make(<microsoft-type-path>, elements: #(#"text"))));
   check-true("<microsoft-locator> test #4", 
  as(<string>, as(<microsoft-locator>, "e:\\foo\\bar\\bax.txt"))
    = "e:\\foo\\bar\\bax.txt");
   check-true("<microsoft-locator> test #5", 
  as(<string>, as(<microsoft-locator>, "\\foo\\bar\\bax.txt"))
    = "\\foo\\bar\\bax.txt");
   check-true("<microsoft-locator> test #6", 
  as(<string>, as(<microsoft-locator>, "foo\\bar\\bax.txt"))
    = "foo\\bar\\bax.txt");
   check-true("<microsoft-locator> test #7", 
  as(<microsoft-locator>, "c:foo\\bar\\bax.txt")
    = make(<ms-dos-file-locator>, volume: "c", directory: make(<microsoft-directory-path>, elements: #("foo", "bar"), relative-path?: #t), base: "bax", type: make(<microsoft-type-path>, elements: #(#"text"))));
   check-true("<microsoft-locator> test #8", 
  as(<string>, as(<microsoft-locator>, "c:foo\\bar\\bax.txt"))
    = "c:foo\\bar\\bax.txt");
   check-true("<microsoft-locator> test #9", 
  as(<microsoft-locator>, "\\\\mumble\\foo\\bar\\bax.txt")
    = make(<microsoft-locator>, prefix: "\\\\mumble\\foo\\bar\\", base: "bax", extension: ".txt"));
   check-true("<microsoft-locator> test #10", 
  as(<microsoft-locator>, "e:\\foo\\bar\\bax.txt")
    = make(<microsoft-locator>, prefix: "e:\\foo\\bar\\", name: "bax.txt"));
   check-true("<microsoft-locator> test #11", 
  as(<microsoft-locator>, "c:foo\\bar\\bax.txt")
    = make(<microsoft-locator>, prefix: "c:foo\\bar\\", name: "bax.txt"));
end test MICROSOFT-LOCATOR-TESTS;

define test DOC-EXAMPLES-TESTS (title: "Doc examples tests")
  /*
     #"postscript"
       = locator-type (as (<locator>, "thesis.ps"))
     "thesis.ps"
       = as (<string>, make (<locator>, base: "thesis", type: #"postscript"))
  */
  check-true("Doc examples tests #1", 
     as (<locator>, "/usr/bin") = as (<file-locator>, "/usr/bin/"));
  check-true("Doc examples tests #2", 
     as (<locator>, "/usr/bin/") = as (<directory-locator>, "/usr/bin"));
  check-true("Doc examples tests #3", 
     "/usr/local/bin/.login"
       = as (<string>, override-locator (as (<posix-locator>, "/usr/local/bin/emacs"),
					 name: ".login")));
  check-true("Doc examples tests #4", 
     "/usr/bin" = as (<string>, simplify-locator (as (<locator>, "/usr/./bin"))));
end test DOC-EXAMPLES-TESTS;


