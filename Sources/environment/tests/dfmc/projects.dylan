Module:    dfmc-environment-test-suite
Synopsis:  DFMC Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $test-application = "environment-test-application";
define constant $test-library     = "environment-test-library";

define constant $test-application-id
  = make(<library-id>, name: $test-application);

define constant $test-application-module-id
  = make(<module-id>, name: $test-application, library: $test-application-id);

define constant $test-library-id
  = make(<library-id>, name: $test-library);

define constant $test-library-module-id
  = make(<module-id>, name: $test-library, library: $test-library-id);

define constant $test-class-id
  = make(<definition-id>, 
	 name: "<test-object>", 
	 module: $test-library-module-id);

define constant $test-internal-class-id
  = make(<definition-id>, 
	 name: "<internal-test-object>", 
	 module: $test-library-module-id);


/// Test suite initialization

define variable *test-application* :: false-or(<project-object>) = #f;
define variable *test-library* :: false-or(<project-object>) = #f;

define function root-directory
    () => (directory :: false-or(<string>))
  let filename = application-filename();
  if (filename)
    let locator = as(<file-locator>, filename);
    let bin-directory = locator.locator-directory;
    let release-locator = locator.locator-directory;
    as(<string>, release-locator)
  end
end function root-directory;

define function test-project-location
    (name :: <string>) => (location :: <locator>)
  let directory = root-directory();
  as(<file-locator>,
     format-to-string
       ("%s/sources/environment/tests/%s/%s.hdp",
	directory,
	select (name by \=)
	  "environment-test-application" => "test-application";
	  "environment-test-library"     => "test-library";
	end,
	name))
end function test-project-location;

define function open-test-projects () => ()
  let library = open-project(test-project-location($test-library));
  open-project-compiler-database
    (library, error-handler: project-condition-handler);
  let application = open-project(test-project-location($test-application));
  open-project-compiler-database(application);
  format-out("Building %s",
	     environment-object-primitive-name(application, application));
  build-project(application,
		clean?: #t,
		error-handler: project-condition-handler,
		progress-callback: method (#rest args)
				     ignore(args);
				     format-out(".")
				   end);
  format-out("\n");
  *test-library*     := library;
  *test-application* := application;
end function open-test-projects;

define function close-test-projects () => ()
  close-project(*test-library*);
  close-project(*test-application*);
end function close-test-projects;

define function project-condition-handler
    (type :: <symbol>, message :: <string>) => ()
  format-out("\nProject warning: type %s: %s\n", type, message)
end function project-condition-handler;


/// Project tests

define test open-projects-test ()
  check-true("Application project open",
	     instance?(*test-application*, <project-object>));
  check-equal("Application project target type",
	      project-target-type(*test-application*),
	      #"executable");
  check-equal("Application project interface type",
	      project-interface-type(*test-application*),
	      #"gui");
  check-equal("Application project name",
	      environment-object-primitive-name
		(*test-application*, *test-application*),
	      $test-application);
  check-true("Library project open",
	     instance?(*test-library*, <project-object>));
  check-equal("Library project target type",
	      project-target-type(*test-library*),
	      #"dll");
  check-equal("Library interface target type",
	      project-interface-type(*test-library*),
	      #"console");
  check-equal("Library project name",
	      environment-object-primitive-name
		(*test-library*, *test-library*),
	      $test-library);
end test open-projects-test;


/// projects suite

define suite projects-suite ()
  test open-projects-test;
end suite projects-suite;
