Module:    testworks-plus
Filename:  testworks-plus.dylan
Author:    Andrew Armstrong (andrewa), Shri Amit(amit)
Synopsis:  Supporting objects for run-test-application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// constants for various application exit values

define constant $HELP                     = 1;
define constant $NO-ARGUMENT-VALUE        = 2;
define constant $SUITE-NOT-FOUND          = 3;
define constant $TEST-NOT-FOUND           = 4;
define constant $INVALID-REPORT-FUNCTION  = 5;
define constant $INVALID-COMMAND-LINE-ARG = 6;
define constant $INVALID-DEBUG-OPTION     = 7;

// Encapsulates the components to be ignored

define class <perform-criteria> (<perform-options>)
  slot perform-ignore :: <stretchy-vector>, 
    init-keyword: ignore:;
end class <perform-criteria>;

define method execute-component? 
    (component :: <component>, options :: <perform-criteria>) 
 => (answer :: <boolean>)
  next-method()
     & ~member?(component, options.perform-ignore)
end method execute-component?;

define method application-error
    (return-code :: <integer>, format-string :: <string>, #rest args) => ()
  apply(format-out, format-string, args);
  exit-application(return-code);
end method application-error;

define method application-warning
    (return-code :: <integer>, format-string :: <string>, #rest args) => ()
  apply(format-out, format-string, args);
end method application-warning;

define method find-component
    (suite-name :: false-or(<string>), test-name :: false-or(<string>))
 => (test :: <component>)
  let suite
    = if (suite-name)
	find-suite(suite-name)
	  | application-error($SUITE-NOT-FOUND, "No such suite %s\n", suite-name);
      end;
  let test
    = if (test-name)
	find-test(test-name, search-suite: suite | root-suite())
	  | application-error($TEST-NOT-FOUND, "No such test %s\n", test-name);
      end;
  test | suite;
end method find-component;

define method find-component
    (suite-names :: false-or(<sequence>), test-names :: false-or(<sequence>))
 => (tests :: <sequence>)
  let tests = make(<stretchy-vector>);
  suite-names 
    & for (name in suite-names)
	add!(tests, find-component(name, #f));
      end for;
  test-names 
    & for (name in test-names)
	add!(tests, find-component(#f, name));
      end for;
  values(tests);
end method find-component;

define method display-run-options
    (start-suite :: <component>,
     report-function :: <function>, 
     options :: <perform-criteria>)
 => ()
  format-out
     ("\nRunning %s %s, with options:\n"
	"   progress-function: %s\n"
	"     report-function: %s\n"
	"              debug?: %s\n"
	"              ignore:%s\n\n",
      if (instance?(start-suite, <suite>)) "suite" else "test" end,
      component-name(start-suite),
      select (options.perform-progress-function)
	full-progress-function => "full";
	null-progress-function => "none";
      end,
      select (report-function)
	full-report-function     => "full";
	failures-report-function => "failures";
	summary-report-function  => "summary";
	null-report-function     => "none";
	log-report-function      => "log";
      end,
      select (options.perform-debug?)
        #"crashes" => "crashes";
        #t         => "failures";
        otherwise  => "no";
      end,
      reduce(method (s :: <string>, c :: <component>)
	       concatenate(s, component-name(c), " ")
	     end method,
	     " ",
	     options.perform-ignore))
end method display-run-options;

