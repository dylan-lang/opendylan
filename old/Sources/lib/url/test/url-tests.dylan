Module: url-test-suite
Author: James Casey
Synopsis: URL test suite. Uses lots of tests from draft-fielding-syntax-03
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// helper method to make a url

define method make-url(what :: <byte-string>)
 =>(retval :: <url>)
  let foo = make(<url>,  url: what);
  foo
end method make-url;

define method check-parse(what :: <byte-string>)
 =>(retval :: <byte-string>)
  url-string(make-url(what))
end method check-parse;

define test extra-characters (description: "Check if we string off extra characters")
  check-equal("Empty URL", 
	      check-parse(""),
	      "");
  check-equal("Uppercase scheme", 
	      check-parse("HTTP://webhost/Home.html?a=b#ref"), 
              "http://webhost/Home.html?a=b#ref");
  check-equal("Whitespace before", 
	      check-parse("  http://webhost/Home.html?a=b#ref"), 
              "http://webhost/Home.html?a=b#ref");
  check-equal("Whitespace after", 
	      check-parse("http://webhost/Home.html?a=b#ref  "), 
              "http://webhost/Home.html?a=b#ref");
  check-equal("Whitespace before and after", 
	      check-parse(" http://webhost/Home.html?a=b#ref   "), 
              "http://webhost/Home.html?a=b#ref");
  check-equal("Lowercase url: prefix", 
	      check-parse("url:http://webhost/Home.html?a=b#ref"), 
              "http://webhost/Home.html?a=b#ref");
  check-equal("Uppercase URL: prefix", 
	      check-parse("URL:http://webhost/Home.html?a=b#ref"), 
              "http://webhost/Home.html?a=b#ref");
end test extra-characters;

define test generic-missing-components(description: "We should be able to leave out components")
  check-equal("No Scheme", 
	      check-parse("//webhost/Home.html?a=b#ref"),
              "//webhost/Home.html?a=b#ref");
  check-equal("No server", 
	      check-parse("http:/Home.html?a=b#ref"),
              "http:/Home.html?a=b#ref");
  check-equal("No Path", 
	      check-parse("http://webhost/?a=b#ref"),
              "http://webhost/?a=b#ref");
  check-equal("No Query", 
	      check-parse("http://webhost/Home.html#ref"),
              "http://webhost/Home.html#ref");
  check-equal("No Fragment", 
	      check-parse("http://webhost/Home.html?a=b"),
              "http://webhost/Home.html?a=b");
end test generic-missing-components;

define test path-checks(description: "Check what path component is set to")

  check-equal("Leading slash",
	      url-path(make-url("/Home.html")),
	      "/Home.html");
  check-equal("No leading slash",
	      url-path(make-url("Home.html")),
	      "Home.html");
  check-equal("Server with trailing slash",
	      url-path(make-url("//webhost/")),
	      "/");
  check-equal("Server with no trailing slash",
	      url-path(make-url("//webhost")),
	      "");
end test path-checks;

define test make-relative-url(description: "Situations where relative urls can be interpreted as absolute ones")
  check-equal("Colon in path",
	      url-path(make-url("./this:that")),
	      "./this:that");
  check-equal("Colon in Fragment",
	      url-fragment(make-url("#ref:foo")),
	      "ref:foo");
  check-equal("Colon in query",
	      url-query(make-url("?query=foo:1")),
	      "query=foo:1");
end test make-relative-url;

define test server-segment-components(description: "Check if we split up the server segment properly, and if we can modify components in it")

  let foo = make-url("ftp://jamesc:foo@webhost:80/Home.html");
  check-equal("User component",
	      foo.url-user,
	      "jamesc");
  check-equal("Password component",
	      foo.url-password,
	      "foo");
  check-equal("Host component",
	      foo.url-host,
	      "webhost");
  check-equal("Port component",
	      foo.url-port,
	      80);

  foo := make-url("ftp://jamesc@webhost/Home.html");
  check-false("Non defined password component",
	      foo.url-password);
  foo := make-url("ftp://jamesc:@webhost/Home.html");
  check-equal("Empty Password component",
	      foo.url-password,
	      "");
  // Get rid of empty password
  foo.url-password := #f;
  check-equal("Undefining a defined password component",
	      foo.url-server,
	      "jamesc@webhost");
  // Get rid of server segment by deleting all server components
  // XXX --- This has some surprising results if we want to always return a correct URL... 
  //  * If we have only a port defined, we should return ""
  //  * If we have only a password, we should return ""
  //

  foo.url-server := "jamesc:foo@webhost:80";
  check-equal("Setting server segment",
	      foo.url-server,
	      "jamesc:foo@webhost:80");
  foo.url-user := #f;
  check-equal("Removing user component",
              foo.url-server,
              "webhost:80");
  foo.url-password := #f;
  check-equal("Removing password component",
              foo.url-server,
              "webhost:80");
  foo.url-host := #f;
  check-equal("Removing host component",
              foo.url-server,
              "");
  foo.url-port := #f;
  check-equal("Removing port component",
              foo.url-server,
              #f);
  check-false("Unsetting all components of server segemnt",
	      foo.url-server);
end test server-segment-components;
  
define test ftp-url-segments(description: "Check validity of ftp url segments")
  let foo = make-url("ftp://jamesc@webhost/Home.html");
  check-equal("FTP URL, username",
	      "jamesc",
	      foo.url-username);

end test ftp-url-segments;

define test http-url-segments(description: "Check validity of http url segments")
  let foo = make-url("http://jamesc@webhost/Home.html");
  check-false("HTTP URL, username",
	      foo.url-username);
end test http-url-segments;

define suite url-test-suite(description: "Tests for the URL object")
  test extra-characters;
  test generic-missing-components;
  test path-checks;
  test make-relative-url;
  test server-segment-components;
  test ftp-url-segments;
  test http-url-segments;
end suite;

