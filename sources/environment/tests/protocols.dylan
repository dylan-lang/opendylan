Module:    environment-test-suite
Synopsis:  Environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ID testing

define constant $test-library-name             = "test-library";
define constant $test-module-name              = "test-module";
define constant $test-definition-name          = "test-definition";
define constant $test-method-name              = "test-method";
define constant $test-method-specializer-names = #["spec1", "spec2"];

define function test-library-id
    () => (id :: <library-id>)
  make(<library-id>, name: $test-library-name)
end function test-library-id;

define function test-module-id
    () => (id :: <module-id>)
  make(<module-id>, name: $test-module-name, library: test-library-id())
end function test-module-id;

define function test-definition-id
    () => (id :: <definition-id>)
  make(<definition-id>, name: $test-definition-name, module: test-module-id())
end function test-definition-id;

define test library-ids-test ()
  check-equal("library id name",
              id-name(test-library-id()),
              $test-library-name);
  check-equal("library interning",
              test-library-id(), test-library-id());
  check-true("make(<library-id>, name: \"dylan\") == $dylan-library-id",
             make(<library-id>, name: "dylan") == $dylan-library-id);
end test library-ids-test;

define test module-ids-test ()
  check-equal("module id name",
              id-name(test-module-id()),
              $test-module-name);
  check-equal("module id library",
              id-library(test-module-id()),
              test-library-id());
  check-equal("module interning",
              test-module-id(), test-module-id());
  check-true("make(<module-id>, name: \"dylan\", ...) == $dylan-module-id",
             make(<module-id>,
                  name: "dylan",
                  library: make(<library-id>, name: "dylan"))
               == $dylan-module-id);
end test module-ids-test;

define test definition-ids-test ()
  check-equal("definition id name",
              id-name(test-definition-id()),
              $test-definition-name);
  check-equal("definition id module",
              id-module(test-definition-id()),
              test-module-id());
  check-equal("definition interning",
              test-definition-id(), test-definition-id());
  check-true("make(<definition-id>, name: \"<object>\", ...) == $<object>-id",
             make(<definition-id>,
                  name: "<object>",
                  module: make(<module-id>,
                               name: "dylan",
                               library: make(<library-id>, name: "dylan")))
               == $<object>-id);
end test definition-ids-test;

define test name-parsing-test ()
  check-equal("library name parsing",
              parse-environment-object-name
                (format-to-string("library %s", $test-library-name)),
              test-library-id());
  check-equal("library name parsing (2)",
              parse-environment-object-name("library dylan"),
              $dylan-library-id);
  check-equal("module name parsing",
              parse-environment-object-name
                (format-to-string("module %s", $test-module-name),
                 library: test-library-id()),
              test-module-id());
  check-equal("qualified module name parsing",
              parse-environment-object-name("module dylan:dylan"),
              $dylan-module-id);
  check-equal("definition name parsing",
              parse-environment-object-name("<object>:dylan:dylan"),
              $<object>-id);
  check-equal("definition name parsing (with library)",
              parse-environment-object-name
                ("<object>:dylan",
                 library: $dylan-library-id),
              $<object>-id);
  check-equal("definition name parsing (with wrong library)",
              parse-environment-object-name
                ("<object>:dylan:dylan",
                 library: test-library-id()),
              $<object>-id);
  check-equal("definition name parsing (with module)",
              parse-environment-object-name
                ("<object>",
                 module: $dylan-module-id),
              $<object>-id);
  check-equal("definition name parsing (with wrong module)",
              parse-environment-object-name
                ("<object>:dylan:dylan",
                 module: test-module-id()),
              $<object>-id);
end test name-parsing-test;

define suite ids-suite ()
  test library-ids-test;
  test module-ids-test;
  test definition-ids-test;
  test name-parsing-test;
end suite ids-suite;


/// Environment protocols suite

define suite environment-protocols-suite ()
  suite ids-suite;
end suite environment-protocols-suite;
