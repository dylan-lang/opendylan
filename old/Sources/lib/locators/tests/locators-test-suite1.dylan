Module:    locators-test-suite
Author:    Tim McNerney
Synopsis:  tests for the locators library
Revision:  September 20 1996 by amit. TW+ & Webster compatible
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//define test CHECK-NATIVE-IS-POSIX ()
//  check-true ("Locator tests assume <native-locator> = <posix-locator>",
//		     <native-locator> = <posix-locator>)
//end;
define test PATH-TESTS (title: "Path tests")
  check-true("make a posix-directory-path", 
    make(<posix-directory-path>, elements: #("foo", "bar", #"wild"))
	= locator-directory(as(<posix-locator>, "/foo/bar/*/")));
end test PATH-TESTS;

define test PATH-MATCH-TESTS (title: "Path match tests")
   check-true("match a locator-directory with <match-path>", 
   match(locator-directory(as(<locator>,"/")), locator-directory(as(<locator>,"/")))
	= make(<match-path>, elements: #()));
   check-true("another <match-path> test", 
   match(locator-directory(as(<locator>,"/foo/bar/baz/")), 
	 locator-directory(as(<locator>,"/foo/bar/baz/")))
		= make(<match-path>, elements: #()));
   check-true("this match should fail", 
   match(locator-directory(as(<locator>,"/foo/bar/")), locator-directory(as(<locator>,"/foo/bar/baz/")))
        = #"fail");
   check-true("Does match fail when it is supposed to?", 
   match(locator-directory(as(<locator>,"/foo/bar/baz/")), locator-directory(as(<locator>,"/foo/bar/")))
	= #"fail");
   check-true("Match a locator-directory with a wildcard", 
   match(locator-directory(as(<locator>,"/foo/*/")), locator-directory(as(<locator>,"/foo/bar/")))
        = make(<match-path>, elements: #("bar")));
   check-true("Match locator-directory with nested wildcards", 
   match(locator-directory(as(<locator>,"/foo/*/*/")), locator-directory(as(<locator>,"/foo/bar/baz/")))
        = make(<match-path>, elements: #("bar", "baz")));
   check-true("Match locator-directory with 2 wildcards", 
   match(locator-directory(as(<locator>,"/foo/**/")), locator-directory(as(<locator>,"/foo/bar/baz/")))
        = make(<match-path>, elements: #("bar", "baz")));

   // Need some illegal cases, e.g. ** followed by **, and ** followed by *
end test PATH-MATCH-TESTS;

define test PATH-INSTANTIATE-TESTS (title: "Path instantiate tests")

   let match-path-0 = make(<match-path>, elements: #());
   let match-path-1 = make(<match-path>, elements: #("bar"));
   let match-path-2 = make(<match-path>, elements: #("bar", "baz"));

   check-true("instantiate a <locator>", 
   instantiate(locator-directory(as(<locator>,"/mumble/")), #f)
	= locator-directory(as(<locator>,"/mumble/")));
   check-condition("should signal a locator error", 
		   <locator-error>,
		   instantiate(locator-directory(as(<locator>,"/*/second/")), 
			       match-path-0));
   check-true("instantiate locator with match-path-1 ", 
   instantiate(locator-directory(as(<locator>,"/*/second/")), match-path-1)
        = locator-directory(as(<locator>,"/bar/second/")));
   check-true("another instantiate locator with match-path-1", 
   instantiate(locator-directory(as(<locator>,"/first/*/third/")), match-path-1)
        = locator-directory(as(<locator>,"/first/bar/third/")));
   check-true("instantiate locator with match-path-2", 
   instantiate(locator-directory(as(<locator>,"/first/*/")), match-path-2)
        = locator-directory(as(<locator>,"/first/bar/")));
   check-true("another instantiate locator with match-path-2", 
   instantiate(locator-directory(as(<locator>,"/first/*/*/")), match-path-2)
        = locator-directory(as(<locator>,"/first/bar/baz/")));
   check-true("Yet another instantiate <locator> with match-path-2", 
   instantiate(locator-directory(as(<locator>,"/first/*/third/*/")), match-path-2)
        = locator-directory(as(<locator>,"/first/bar/third/baz/")));
   check-condition("Signals a locator error", 
		   <locator-error>,
		   instantiate(locator-directory(as(<locator>,"/first/*/third/*/")),
			       match-path-1));
end test PATH-INSTANTIATE-TESTS;

define test MORE-PATH-INSTANTIATE-TESTS (title: "More path instantiate tests")

   let match-path-0 = make(<match-path>, elements: #());
   let match-path-1 = make(<match-path>, elements: #("bar"));
   let match-path-2 = make(<match-path>, elements: #("bar", "baz"));

   check-true("<locator> instantiation with no match-path", 
   instantiate(locator-directory(as(<locator>,"/**/")), match-path-0)
        = locator-directory(as(<locator>,"/")));
   check-true("<locator> instantiation with one match-path", 
   instantiate(locator-directory(as(<locator>,"/**/")), match-path-1)
        = locator-directory(as(<locator>,"/bar/")));
   check-true("<locator> instantiation with two match-path's", 
   instantiate(locator-directory(as(<locator>,"/**/")), match-path-2)
        = locator-directory(as(<locator>,"/bar/baz/")));
   check-true("Another <locator> instantiation with no match-path", 
   instantiate(locator-directory(as(<locator>,"/**/first/second/third/forth/")), match-path-0)
        = locator-directory(as(<locator>,"/first/second/third/forth/")));
   check-true("Yet another <locator> instantiation with no match-path", 
   instantiate(locator-directory(as(<locator>,"/first/**/second/third/forth/")), match-path-0)
        = locator-directory(as(<locator>,"/first/second/third/forth/")));
   check-true("A <locator> with match-path-0 (still going)", 
   instantiate(locator-directory(as(<locator>,"/first/second/**/")), match-path-0)
        = locator-directory(as(<locator>,"/first/second/")));
   check-true("An instance of <locator> with a single match path", 
   instantiate(locator-directory(as(<locator>,"/**/second/third/forth/")), match-path-1)
        = locator-directory(as(<locator>,"/bar/second/third/forth/")));
   check-true("An instance of <locator> with match-path-1", 
   instantiate(locator-directory(as(<locator>,"/first/**/third/forth/")), match-path-1)
        = locator-directory(as(<locator>,"/first/bar/third/forth/")));
   check-true("Another instance of <locator> with match-path-1", 
   instantiate(locator-directory(as(<locator>,"/first/second/**/")), match-path-1)
        = locator-directory(as(<locator>,"/first/second/bar/")));
   check-true("An instance of <locator> with match-path-2", 
   instantiate(locator-directory(as(<locator>,"/**/third/forth/")), match-path-2)
        = locator-directory(as(<locator>,"/bar/baz/third/forth/")));
   check-true("Another instance of <locator> with match-path-2", 
   instantiate(locator-directory(as(<locator>,"/first/**/forth/")), match-path-2)
        = locator-directory(as(<locator>,"/first/bar/baz/forth/")));
   check-true("Finally the final instance of <locator> with match path 2", 
   instantiate(locator-directory(as(<locator>,"/first/second/**/")), match-path-2)
        = locator-directory(as(<locator>,"/first/second/bar/baz/")));
end test MORE-PATH-INSTANTIATE-TESTS;


define test POSIX-LOCATOR-TESTS (title: "<posix-locator> tests")
     check-true("make a <posix-locator>", 
    as(<posix-locator>, "/foo/bar/*")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #("foo", "bar")), base: #"wild", type: #f));
   check-true("make a <posix-directory-locator>", 
    as(<posix-locator>, "../../")
	= make(<posix-directory-locator>, directory: make(<posix-directory-path>, elements: #(#"parent", #"parent"), relative-path?: #t), base: #f, type: #f));
   check-true("make a <posix-file-locator>", 
    as(<posix-locator>, "../../.login")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #(#"parent", #"parent"), relative-path?: #t), base: ".login", type: #f));
   check-true("make another <posix-file-locator>", 
    as(<posix-locator>, "../.login")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #(#"parent"), relative-path?: #t), base: ".login", type: #f));
   check-true("make <posix-file-locator> with relative-path?", 
    as(<posix-locator>, "**/.login")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #(#"wild-inferiors"), relative-path?: #t), base: ".login", type: #f));
   check-true("Instantiate another <posix-file-locator>", 
    as(<posix-locator>, "*/.login")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #(#"wild"), relative-path?: #t), base: ".login", type: #f));
   check-true("Instantiate <posix-file-locator>", 
    as(<posix-locator>, ".login")
	= make(<posix-file-locator>, base: ".login", type: #f));
   check-true("Instantiate another <posix-file-locator>", 
    as(<posix-locator>, "bar/.login")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #("bar"), relative-path?: #t), base: ".login", type: #f));
   check-true("Make a <posix-file-locator> as a <posix-locator>", 
    as(<posix-locator>, "/bar/.login") 
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #("bar")), base: ".login", type: #f));
   check-true("instance of <posix-file-locator> with a type: <posix-type-path>", 
    as(<posix-locator>, "/bar/baz.dylan")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #("bar")), base: "baz", type: make(<posix-type-path>, elements: #(#"dylan"))));
   check-true("Yet another instantiation of <posix-file-locator>", 
    as(<posix-locator>, "/.login")
	= make(<posix-file-locator>, directory: make(<posix-directory-path>, elements: #()), base: ".login", type: #f));
   check-true("make a simple <posix-locator>", 
    as(<posix-locator>, "/foo/bar/*")
	= make(<posix-locator>, prefix: "/foo/bar/", name: "*"));
   check-true("make another <posix-locator>, with a prefix:", 
    as(<posix-locator>, "../../")
	= make(<posix-locator>, prefix: "../../"));
   check-true("make another <posix-locator>, with a prefix: & a name:", 
    as(<posix-locator>, "../../.login")
	= make(<posix-locator>, prefix: "../../", name: ".login"));
   check-true("make another <posix-locator>, with a relative-path?: ", 
    as(<posix-locator>, "../.login")
	= make(<posix-locator>, directory: make(<posix-directory-path>, elements: #(#"parent"), relative-path?: #t), name: ".login"));
   check-true("Simple instance of <posix-locator> with name: and prefix:", 
    as(<posix-locator>, "**/.login")
	= make(<posix-locator>, prefix: "**/", name: ".login"));
   check-true("Another simple instance of <posix-locator> with name: and prefix:", 
    as(<posix-locator>, "*/.login")
	= make(<posix-locator>, prefix: "*/", name: ".login"));
   check-true("How about without a null prefix", 
    as(<posix-locator>, ".login")
	= make(<posix-locator>, prefix: "", name: ".login"));
   check-true("And now with bar/ prefix:", 
    as(<posix-locator>, "bar/.login")
	= make(<posix-locator>, prefix: "bar/", name: ".login"));
   check-true("Cool -> a <posix-locator> with a base: and extension:", 
    as(<posix-locator>, "/bar/baz.dylan") 
	= make(<posix-locator>, prefix: "/bar/", base: "baz", extension: ".dylan"));
   check-true("Another <posix-locator> with a prefix and yes a name too!", 
    as(<posix-locator>, "/bar/baz.dylan")
	= make(<posix-locator>, prefix: "/bar/", name: "baz.dylan"));
   check-true("I am so glad this is the last <posix-locator> with prefix and a name", 
    as(<posix-locator>, "/.login")
	= make(<posix-locator>, prefix: "/", name: ".login"));
end test POSIX-LOCATOR-TESTS;

define test ABSTRACT-LOCATOR-TESTS (title: "<abstract-locator> tests")
     check-true("make an <abstract-locator>", 
    as(<locator>, "abstract://foo/bar/baz.dylan;newest")
	= make(<abstract-locator>, host: make(<host-path>, elements: #("foo")), 
	       directory: make(<abstract-directory-path>, elements: #("bar")), 
	       base: "baz", type: make(<posix-type-path>, elements: #(#"dylan")), version: #"newest"));
   check-true("make an <abstract-file-locator>", 
    as(<locator>, "abstract://foo.quux/bar/mumble/fun.dylan")
	= make(<abstract-file-locator>, host: make(<host-path>, elements: #("foo", "quux")), 
	       directory: make(<abstract-directory-path>, elements: #("bar", "mumble")), 
	       base: "fun", type: make(<posix-type-path>, elements: #(#"dylan")), version: #f));
   check-true("make another <abstract-file-locator>", 
    as(<locator>, "abstract://foo.quux/bar/mumble/fun")
	= make(<abstract-file-locator>, host: make(<host-path>, elements: #("foo", "quux")), directory: make(<abstract-directory-path>, elements: #("bar", "mumble")), base: "fun", type: #f, version: #f));
   check-true("make an <abstract-directory-locator>", 
    as(<locator>, "abstract://foo.quux/bar/mumble/")
	= make(<abstract-directory-locator>, host: make(<host-path>, elements: #("foo", "quux")), directory: make(<abstract-directory-path>, elements: #("bar", "mumble")), base: #f, type: #f, version: #f));
   check-true("make an <abstract-file-locator> with an #oldest version", 
    as(<locator>, "abstract://foo.quux/bar/mumble/foo;oldest")
	= make(<abstract-file-locator>, host: make(<host-path>, elements: #("foo", "quux")), directory: make(<abstract-directory-path>, elements: #("bar", "mumble")), base: "foo", type: #f, version: #"oldest"));
   check-true("How about another <abstract-file-locator>", 
    as(<locator>, "abstract://foo.quux/bar")
	= make(<abstract-file-locator>, host: make(<host-path>, elements: #("foo", "quux")), directory: make(<abstract-directory-path>, elements: #()), base: "bar", type: #f, version: #f));
   check-true("Yet another <abstract-file-locator> with some funky keywords", 
    as(<locator>, "abstract://foo.quux/bar/mumble/*.dylan;oldest")
	= make(<abstract-file-locator>, host: make(<host-path>, elements: #("foo", "quux")), directory: make(<abstract-directory-path>, elements: #("bar", "mumble")), base: #"wild", type: make(<posix-type-path>, elements: #(#"dylan")), version: #"oldest"));
   check-true("An instance of <abstract-directory-locator>", 
    as(<locator>, "abstract://foo/bar/")
	= make(<abstract-directory-locator>, host: make(<host-path>, elements: #("foo")), directory: make(<abstract-directory-path>, elements: #("bar")), base: #f, type: #f, version: #f));
   check-true("An <abstract-locator> instance with a base: and extension:", 
    as(<locator>, "abstract://foo/bar/baz.dylan;newest")
	= make(<abstract-locator>, prefix: "//foo/bar/", base: "baz", extension: ".dylan", version: #"newest"));
   check-true("a <locator> as an <abstract-locator>", 
    as(<abstract-locator>, "//foo.quux/bar/mumble/fun.dylan")
	= make(<locator>, prefix: "abstract://foo.quux/bar/mumble/", name: "fun.dylan"));
   check-true("an <abstract-locator> as an <abstract-locator>", 
    as(<abstract-locator>, "//foo.quux/bar/mumble/fun")
	= make(<abstract-locator>, host: make(<host-path>, elements: #("foo", "quux")), directory: make(<abstract-directory-path>, elements: #("bar", "mumble")), name: "fun", version: #f));
   check-true("a simple <locator> with a prefix:", 
    as(<locator>, "abstract://foo.quux/bar/mumble/")
	= make(<locator>, prefix: "abstract://foo.quux/bar/mumble/"));
   check-true("another simple <locator> with a name: and a version:", 
    as(<abstract-locator>, "//foo.quux/bar/mumble/foo;oldest")
	= make(<locator>, prefix: "abstract://foo.quux/bar/mumble/", name: "foo", version: #"oldest"));
   check-true("an <abstract-locator> with elements: and directory:", 
    as(<abstract-locator>, "//foo.quux/bar")
	= make(<abstract-locator>, host: make(<host-path>, elements: #("foo", "quux")), directory: make(<abstract-directory-path>, elements: #()), name: "bar"));
   check-true("another <abstract-locator> as a <locator>", 
    as(<locator>, "abstract://foo.quux/bar/mumble/*.dylan;oldest")
	= make(<abstract-locator>, prefix: "//foo.quux/bar/mumble/", name: "*.dylan", version: #"oldest"));
   check-true("the final and the inverse of the above - cool huh!", 
    as(<abstract-locator>, "//foo/bar/")
	= make(<locator>, prefix: "abstract://foo/bar/"));
end test ABSTRACT-LOCATOR-TESTS;

define test EQUIVALENCE-TESTS (title: "= tests")
     check-true("Instantiate a <posix-directory-path> with lots of slots", 
    (make(<posix-directory-path>, elements: #("foo", "bar", #"wild")) = make(<posix-directory-path>, elements: #("foo", "bar", #"wild"), relative-path?: #t))
	== #f);
   check-true("Are two identical <posix-locators> really identical?", 
    (as(<posix-locator>, "../../.login") = as(<posix-locator>, "../../.login"))
	~= #f);
   check-true("Are two different <posix-locators> really different?", 
    (as(<posix-locator>, ".login") = as(<posix-locator>, "/.login"))
	== #f);
end test EQUIVALENCE-TESTS;

define test GENERIC-PARSING-TESTS (title: "Generic parsing tests")
    check-true("A general parsing test", 
    as(<locator>, "abstract://foo/bar/")
	= as(<abstract-locator>, "abstract://foo/bar/") );
end test GENERIC-PARSING-TESTS;

define test COMPOSITE-TYPE-TESTS (title: "Composite type tests")
  check-true("Composite type test", 
  as(<abstract-locator>, "abstract://foo/bar/quux.text.gz;42") 
	= make(<abstract-file-locator>, host: make(<host-path>, elements: #("foo")), directory: make(<abstract-directory-path>, elements: #("bar")), base: "quux", type: make(<posix-type-path>, elements: #(#"text", #"gzip")), version: 42));
end test COMPOSITE-TYPE-TESTS;

define test IP-HOST-TESTS (title: "IP host tests")
  check-true("An IP host test", 
    as(<abstract-locator>, "abstract://18.43.0.152/zu/tim/.login")
	= make(<abstract-file-locator>, host: make(<host-path>, elements: #(18, 43, 0, 152)), directory: make(<abstract-directory-path>, elements: #("zu", "tim")), base: ".login", type: #f, version: #f));
end test IP-HOST-TESTS;

define test POSIX-AS-STRING-TESTS (title: "Posix as <string> tests")
     check-true("Posix-as-string test #1", 
    as(<string>, as(<posix-locator>, "/foo/bar/*"))
	= "/foo/bar/*");
   check-true("Posix-as-string test #2", 
    as(<string>, as(<posix-locator>, "../../"))
	= "../../");
   check-true("Posix-as-string test #3", 
    as(<string>, as(<posix-locator>, "../../.login"))
	= "../../.login");
   check-true("Posix-as-string test #4", 
    as(<string>, as(<posix-locator>, "../.login"))
	= "../.login");
   check-true("Posix-as-string test #5", 
    as(<string>, as(<posix-locator>, "**/.login"))
	= "**/.login");
   check-true("Posix-as-string test #6", 
    as(<string>, as(<posix-locator>, "*/.login"))
	= "*/.login");
   check-true("Posix-as-string test #7", 
    as(<string>, as(<posix-locator>, ".login"))
	= ".login");
   check-true("Posix-as-string test #8", 
    as(<string>, as(<posix-locator>, "bar/.login"))
	= "bar/.login");
   check-true("Posix-as-string test #9", 
    as(<string>, as(<posix-locator>, "/bar/.login"))
	= "/bar/.login");
   check-true("Posix-as-string test #10", 
    as(<string>, as(<posix-locator>, "/bar/baz.dylan"))
	= "/bar/baz.dylan");
   check-true("Posix-as-string test #11", 
    as(<string>, as(<posix-locator>, "/.login"))
	= "/.login");
    // type w/o base
   check-condition("This is a locator error dude!", 
		   <locator-error>,
		   as(<string>,
		      make(<posix-locator>,
			   type: make(<posix-type-path>,
				      elements: #(#"text")))))
end test POSIX-AS-STRING-TESTS;


define test ABSTRACT-AS-STRING-TESTS (title: "Abstract as <string> tests")
     check-true("<abstract-locator> as a <string> test #1", 
    as(<string>, as(<abstract-locator>, "abstract://foo/bar/baz.dylan;newest"))
	= "abstract://foo/bar/baz.dylan;newest");
   check-true("<abstract-locator> as a <string> test #2", 
    as(<string>, as(<abstract-locator>, "abstract://foo.quux/bar/mumble/fun.dylan"))
	= "abstract://foo.quux/bar/mumble/fun.dylan");
   check-true("<abstract-locator> as a <string> test #3", 
    as(<string>, as(<abstract-locator>, "abstract://foo.quux/bar/mumble/fun"))
	= "abstract://foo.quux/bar/mumble/fun");
   check-true("<abstract-locator> as a <string> test #4", 
    as(<string>, as(<abstract-locator>, "abstract://foo.quux/bar/mumble/"))
	= "abstract://foo.quux/bar/mumble/");
   check-true("<abstract-locator> as a <string> test #5", 
    as(<string>, as(<abstract-locator>, "abstract://foo.quux/bar/mumble/foo;oldest"))
	= "abstract://foo.quux/bar/mumble/foo;oldest");
   check-true("<abstract-locator> as a <string> test #6", 
    as(<string>, as(<abstract-locator>, "abstract://foo.quux/bar"))
	= "abstract://foo.quux/bar");
   check-true("<abstract-locator> as a <string> test #7", 
    as(<string>, as(<abstract-locator>, "abstract://foo.quux/bar/mumble/*.dylan;oldest"))
	= "abstract://foo.quux/bar/mumble/*.dylan;oldest");
   check-true("<abstract-locator> as a <string> test #8", 
    as(<string>, as(<abstract-locator>, "abstract://foo/bar/"))
	= "abstract://foo/bar/");
   check-true("<abstract-locator> as a <string> test #9", 
	      as(<string>, make(<abstract-locator>,
				host: make(<host-path>, elements: #("foo")),
				type: make(<posix-type-path>,
					   elements: #(#"text"))))
		= "abstract://foo/.text");
  // no-host
  check-condition("<abstract-locator> as a <string> test #10", 
		  <locator-error>,
		  as(<string>,
		     make(<abstract-locator>,
			  base: "foo",
			  type: make(<posix-type-path>,
				     elements: #(#"text")))))
end test ABSTRACT-AS-STRING-TESTS;

define test LOCATOR-MATCH-TESTS (title: "Locator match tests")
  check-true("Do the <locator>'s match", 
  match(as(<locator>, "abstract://sys/editor/**/*.*;*"), as(<locator>, "abstract://sys/editor/bin/sparc/display.wfasl"))
      = make(<abstract-match>, host: make(<match-path>, elements: #()),
	     directory: make(<match-path>, elements: #("bin", "sparc")),
	     base: "display", type: make(<match-path>, elements: #("wfasl")), version: #f));
   check-true("This <locator> matching should fail for sure", 
  match(as(<locator>, "abstract://sys/editor/**/*.*;*"), as(<locator>, "abstract://sys/compiler/src/reg-alloc.dylan"))
      = #"fail");
end test LOCATOR-MATCH-TESTS;

define test LOCATOR-INSTANTIATE-TESTS (title: "Locator instantiate tests")
  check-true("And still going - another <locator> instantiation", 
  instantiate(as(<locator>, "/proj/dylan/editor/**/*.*"), 
	      match(as(<locator>, "abstract://sys/editor/**/*.*;*"),
                    as(<locator>, "abstract://sys/editor/bin/sparc/display.wfasl")))
      = as(<locator>, "/proj/dylan/editor/bin/sparc/display.wfasl"));
end test LOCATOR-INSTANTIATE-TESTS;