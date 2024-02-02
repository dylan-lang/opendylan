Module:    dfmc-environment-test-suite
Synopsis:  DFMC Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test application-ids-test ()
  let project = *test-application* | error("*test-application* is not set");
  let library = project-library(project) | error("no project-library");
  check-equal("application library id",
              environment-object-id(project, library),
              $test-application-id);
  let modules = library-modules(project, library, imported?: #f);
  check-equal("application has only one module",
              size(modules), 1);
  check-equal("application module id",
              environment-object-id(project, modules[0]),
              $test-application-module-id);
end test application-ids-test;

define test dfmc-library-ids-test ()
  let project = *test-library* | error("*test-library* is not set");
  let library = project-library(project) | error("no project-library");
  check-equal("test library id",
              environment-object-id(project, library),
              $test-library-id);
  let modules = library-modules(project, library, imported?: #f);
  check-equal("test library has two modules",
              size(modules), 2);
  let module-ids = map(curry(environment-object-id, project), modules);
  check-true("test library module id",
             member?($test-library-module-id, module-ids));
end test dfmc-library-ids-test;

define test home-name-test ()
  let project = *test-application* | error("*test-application* is not set");
  let class  = find-environment-object(project, $test-internal-class-id);
  let module
    = find-module(project, id-name($test-application-module-id))
    | error("Can't find module");
  let internal-module
    = find-module(project, id-name($test-library-module-id))
    | error("Can't find module");
  check-instance?("find-environment-object for application module",
                  <module-object>, module);
  check-instance?("find-environment-object for library module",
                  <module-object>, internal-module);
  check-instance?("find-environment-object for internal class",
                  <class-object>, class);
  check-false("internal class has no name in application",
              environment-object-name(project, class, module));
  check-true("internal class has correct home name",
             begin
               let home-name = environment-object-home-name(project, class);
               instance?(home-name, <binding-name-object>)
                 & name-namespace(project, home-name) = internal-module
             end);
end test home-name-test;


/// projects suite

define suite names-suite ()
  test home-name-test;
  test application-ids-test;
  test dfmc-library-ids-test;
end suite names-suite;
