Module:    locators-test-suite
Author:    Tim McNerney
Synopsis:  All the suites for the locators library test suite
Revision:  September 20 1996 by amit. TW+ & Webster compatible
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite basic-locators-test-suite ()
//test CHECK-NATIVE-IS-POSIX;
  test PATH-TESTS;
  test PATH-MATCH-TESTS;
  test PATH-INSTANTIATE-TESTS;
  test MORE-PATH-INSTANTIATE-TESTS;
  test POSIX-LOCATOR-TESTS;
  test ABSTRACT-LOCATOR-TESTS;
  test EQUIVALENCE-TESTS;
  test GENERIC-PARSING-TESTS;
  test COMPOSITE-TYPE-TESTS;
  test IP-HOST-TESTS;
  test POSIX-AS-STRING-TESTS;
  test ABSTRACT-AS-STRING-TESTS;
  test LOCATOR-MATCH-TESTS;
  test LOCATOR-INSTANTIATE-TESTS;
end suite;

define suite more-locators-test-suite ()
  test AS-DIRECTORY-LOCATOR-TESTS;
  test AS-FILE-LOCATOR-TESTS;
  test ABBREVIATE-TESTS;
  test ABBREVIATE-MERGE-TESTS;
  test HTTP-LOCATOR-TESTS;
  test FTP-LOCATOR-TESTS;
  test MERGE-TESTS;
  test MICROSOFT-LOCATOR-TESTS;
  test DOC-EXAMPLES-TESTS;
end suite;

define suite translation-test-suite 
  (setup-function: setup-translation-suite,
   cleanup-function: cleanup-translation-suite)
  test    MATCH-ABSTRACT-HOST-TEST;
  test    FORWARD-TRANSLATION-TESTS;
  test    BACK-TRANSLATION-TESTS;
end suite;

define suite old-locators-test-suite ()
  suite basic-locators-test-suite;
  suite more-locators-test-suite;
  suite translation-test-suite;
end suite;
