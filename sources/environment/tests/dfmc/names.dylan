Module:    dfmc-environment-test-suite
Synopsis:  DFMC Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test application-ids-test ()
  let project = *test-application*;
  let library = project-library(project);
  check-equal("application library id",
	      environment-object-id(project, library),
	      $test-application-id);
  let modules = library-modules(project, library);
  check-equal("application has only one module",
	      size(modules), 1);
  check-equal("application module id",
	      environment-object-id(project, modules[0]),
	      $test-application-module-id);
end test application-ids-test;

define test library-ids-test ()
  let project = *test-library*;
  let library = project-library(project);
  check-equal("test library id",
	      environment-object-id(project, library),
	      $test-library-id);
  let modules = library-modules(project, library);
  check-equal("test library has only one module",
	      size(modules), 2);
  check-equal("test library module id",
	      environment-object-id(project, modules[0]),
	      $test-library-module-id);
end test library-ids-test;

define test home-name-test ()
  let project = *test-application*;
  let class  = find-environment-object(project, $test-internal-class-id);
  let module = find-environment-object(project, $test-application-module-id);
  let internal-module
    = find-environment-object(project, $test-library-module-id);
  check-true("find-environment-object for application module",
	     instance?(module, <module-object>));
  check-true("find-environment-object for library module",
	     instance?(internal-module, <module-object>));
  check-true("find-environment-object for internal class",
	     instance?(class, <class-object>));
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
  test library-ids-test;
end suite names-suite;
