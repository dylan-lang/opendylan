Module:    locators-test-suite
Author:    Tim McNerney
Synopsis:  translation tests for the locators library
Revision:  September 20 1996 by amit. TW+ & Webster compatible
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test MATCH-ABSTRACT-HOST-TEST (title: "Match abstract host test")
  check-true("Match abstract host test",
  match(as(<locator>, "abstract://foo/**/*.*;*"), 
        as(<locator>, "abstract://foo/"))
    ~= #"fail")
end test MATCH-ABSTRACT-HOST-TEST;

define test FORWARD-TRANSLATION-TESTS ()
  check-true("forward translation tests #1",
  as(<physical-locator>, "abstract://sys/editor/bin/redisplay.wfasl")
    = as(<locator>, "/usr/users/dylan/lib/editor/bin/redisplay.wfasl"));
  check-true("forward translation tests #2",
  as(<physical-locator>, "abstract://sys/locators/abstract-locators.dylan")
    = as(<locator>, "/usr/users/tim/dylan/locators/abstract-locators.dylan"));
  check-condition("forward translation tests #3",
		  <locator-error>,
		  as(<physical-locator>,
		     "abstract://sys/compiler/front-end/parse.dylan"))
end test FORWARD-TRANSLATION-TESTS;


define test BACK-TRANSLATION-TESTS ()
  // Note that when the <abstract-pathname> ("from" in the mapping) has a
  // wild version, then this translates to an abstract pathname with a wild
  // version, presumably because the posix match or the candidate can't supply it.
  // FIX THIS!  [fixed -mf]

  check-true("Back translation tests #1",
  as(<abstract-locator>, as(<locator>, "/usr/users/dylan/lib/editor/bin/redisplay.wfasl"))
    = as(<locator>, "abstract://sys/editor/bin/redisplay.wfasl"));
  check-true("Back translation tests #2",
  as(<abstract-locator>, as(<locator>, "/usr/users/tim/dylan/locators/abstract-locators.dylan"))
    = as(<locator>, "abstract://sys/locators/abstract-locators.dylan"));
  check-condition("Back translation tests #3",
		  <locator-error>,
		  as(<abstract-locator>,
		     as(<locator>, "/usr/users/tim/.emacs")))
end test BACK-TRANSLATION-TESTS;

///////////////////////////////////////////////////////////////////////////
// Note that unlike Symbolics logical pathnames, this implementation has
// yet to have any "autoload" feature which will go look for the definition
// of an abstract host if it isn't definined

// Soon you'll be able to abbreviate to this:
/*
  // [this should work now:  -mf]
  add-abstract-host
    ("abstract://sys/",
       #(#("editor/**/*.*;*",   "/usr/users/dylan/lib/editor/**/*.*"),
         #("locators/**/*.*;*", "/usr/users/tim/dylan/locators/**/*.*")));
*/
// Roman wants it even shorter
/*
  // [this should work now:  -mf]
  add-abstract-host ("abstract://sys/", "/usr/users/dylan/**/*.*")
  // or even just this:
  add-abstract-host ("abstract://sys/", "/usr/users/dylan/")
*/


// Example of how abstract versions might map to posix conventions
/*
  add-abstract-host
    ("abstract://sys/",
       #(#("abstract://sys/editor/**/*.*;newest", "/usr/users/dylan/lib/editor/v42/**/*.*"),
         #("abstract://sys/editor/**/*.*;42", "/usr/users/dylan/lib/editor/v42/**/*.*"),
         #("abstract://sys/editor/**/*.*;41", "/usr/users/dylan/lib/editor/v41/**/*.*"),
         #("abstract://sys/editor/**/*.*;40", "/usr/users/dylan/lib/editor/v40/**/*.*"),
         #("abstract://sys/locators/**/*.*;*", "/usr/users/tim/dylan/locators/**/*.*")));
*/

// Example of how to map bins to a different directory
/*
  add-abstract-host
    ("abstract://dylan/",  // Note: Should be case insensitive:  Is it right now?
       #(#("abstract://dylan/editor/**/*.dylan", "/usr/users/dylan/lib/editor/**/src/*.*"),
         #("abstract://dylan/editor/**/*.wfasl", "/usr/users/dylan/lib/editor/**/bin/*.*")));

// This will translate:
  as(<physical-locator>, "abstract://sys/editor/display/capi/redisplay.wfasl")
    = as(<locator>, "/usr/users/dylan/lib/editor/display/capi/bin/redisplay.wfasl")
*/
    
define variable *preserved-host-table* = #f;

define function setup-translation-suite ()
  *preserved-host-table* := *abstract-host-table*; // we know clear doesn't side effect
  clear-abstract-host-table();
  add-abstract-host
    ("abstract://sys/",
     #(#("abstract://sys/editor/**/*.*;*", 
	 "/usr/users/dylan/lib/editor/**/*.*"),
       #("abstract://sys/locators/**/*.*;*",
	 "/usr/users/tim/dylan/locators/**/*.*")));
end function;

define function cleanup-translation-suite ()
  *abstract-host-table* := *preserved-host-table*;
end function;

