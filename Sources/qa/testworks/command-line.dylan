Module:       testworks
Synopsis:     Implementation of run-test-application
Author:       Andy Armstrong, Shri Amit
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
	"              ignore: %s\n\n",
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


/// Argument processing

define constant $keyword-prefixes = #['-', '/'];

define method process-argument
    (argument :: <string>)
 => (text :: <string>, keyword? :: <boolean>)
  if (keyword-argument?(argument))
    values(copy-sequence(argument, start: 1), #t)
  else
    values(argument, #f)
  end
end method process-argument;

define method keyword-argument? 
    (argument :: <string>) => (keyword? :: <boolean>)
  member?(argument[0], $keyword-prefixes)
end method keyword-argument?;

define method argument-value
    (keyword :: <string>, arguments :: <deque>,
     #key allow-no-arguments?)
 => (value :: <stretchy-vector>)
  if (~allow-no-arguments?
      & (empty?(arguments) | keyword-argument?(arguments[0])))
    application-error($NO-ARGUMENT-VALUE,
		      "No argument specified for keyword '%s'.\n"
                      "Use -help for available options\n", 
		      keyword)
  end;
  let value = make(<stretchy-vector>);
  while (~empty?(arguments) & ~keyword-argument?(arguments[0]))
    add!(value, pop(arguments))
  end;
  value
end method argument-value;

define method help-function (appname :: <string>) => ()
  format-out("Application: %s\n"
             "\n"
	     "  Arguments: [-debug | -nodebug]\n"
             "             [-debug [never | failures | crashes]]\n"
	     "             [-quiet]\n"
	     "             [-progress | -noprogress]\n"
	     "             [-report [none | full | failures | summary | log]]\n"
	     "             [-suite <name1> <name2> ... ...]\n"
	     "             [-test <name1> <name2> ... ...]\n"
             "             [-top]\n"
             "             [-profiling]\n"
	     "             [-ignore-suite <name1> <name2> ... ...]\n"
	     "             [-ignore-test <name1> <name2> ... ...]\n",
	     appname);
  exit-application($HELP);
end method help-function;

define method compute-application-options
    (parent :: <component>, 
     arguments :: <sequence>)
 => (quiet? :: <boolean>,
     profiling? :: <boolean>,
     start-suite :: <component>, 
     options :: <perform-criteria>,
     report-function :: <function>)
  let arguments        = as(<deque>, arguments);
  let run-suites       = make(<stretchy-vector>);
  let run-tests        = make(<stretchy-vector>);
  let ignore-tests     = make(<stretchy-vector>);
  let ignore-suites    = make(<stretchy-vector>);
  let report-function  = failures-report-function;
  let options          = make(<perform-criteria>);
  let quiet?           = #f;
  let profiling?       = #f;  // might want to default to #t and have -no-profiling.

  // Parse through the arguments
  while (~empty?(arguments))
    let argument = pop(arguments);
    let (option, keyword?) = process-argument(argument);
    if (keyword?)
      select (option by \=)
	"debug" =>
	  options.perform-debug?
	    := begin
		 let values = argument-value(option, arguments, allow-no-arguments?: #t);
		 select (size(values))
		   0 =>
		     #t;
		   1 =>
		     let debug-option = values[0];
		     select (debug-option by \=)
		       "no"       => #f;
		       "crashes"  => #"crashes";
		       "failures" => #t;
		       otherwise =>
			 application-error($INVALID-DEBUG-OPTION,
					   "Debug option '%s' not supported.\n"
					   "Use -help for available options\n",
					   debug-option);
		     end;
		   otherwise =>
		     application-error($INVALID-DEBUG-OPTION,
				       "More than one debug option supplied.\n"
				       "Use -help for available options.\n",
				       values)
		 end
	       end;
	"nodebug" => 
	  options.perform-debug? := #f;
	"progress" => 
	  options.perform-progress-function := full-progress-function;
	"noprogress" => 
	  options.perform-progress-function := null-progress-function;
	"report" =>
	  report-function
	    := begin
		 let function-name = pop(arguments);
		 select (function-name by \=)
		   "none"     => null-report-function;
		   "full"     => full-report-function;
		   "summary"  => summary-report-function;
		   "failures" => failures-report-function;
		   "log"      => log-report-function;
		   otherwise =>
		     application-error($INVALID-REPORT-FUNCTION,
				       "Report function '%s' not supported.\n"
				       "Use -help for available options\n",
				       function-name);
		 end select
	       end;
	"suite" =>
	  run-suites
	    := concatenate(run-suites, argument-value(option, arguments));
	"test" =>
	  run-tests 
	    := concatenate(run-tests, argument-value(option, arguments));
        "top" =>
	  run-suites
	    := add!(run-suites, component-name(parent));
        "profiling" =>
          profiling? := #t;
	"ignore-suite" =>
	  ignore-suites 
	    := concatenate(ignore-suites, argument-value(option, arguments));
	"ignore-test" =>
	  ignore-tests 
	    := concatenate(ignore-tests,  argument-value(option, arguments));
	"quiet" =>
	  quiet? := #t;
	"verbose" =>
	  quiet? := #f;
	otherwise =>
	  application-warning($INVALID-COMMAND-LINE-ARG,
			      "Unknown command line keyword '%s': leaving for application.\n"
			      "Use -help for available options\n",
			      option);
	  // discard any options to the invalid keyword
	  while (~empty?(arguments) & ~keyword-argument?(arguments[0]))
	    pop(arguments)
	  end;
      end;
    else
      application-warning($INVALID-COMMAND-LINE-ARG,
			  "Ignoring unexpected command line argument '%s'.\n"
			  "Use -help for available options\n",
			  option);
    end
  end while;

  // Determine the starting suite or test
  let start-suite
    = case
	run-suites.size = 0 & run-tests.size = 0 =>
	  parent;
	run-suites.size = 1 & run-tests.size = 0 =>
	  find-component(run-suites[0], #f);
	run-suites.size = 0 & run-tests.size = 1 =>
	  find-component(#f, run-tests[0]);
	otherwise =>
	  make(<suite>,
	       name: "Specified Components",
	       description: "arguments to -suite and -test",
	       components: find-component(run-suites, run-tests));
      end case;
  options.perform-ignore := find-component(ignore-suites, ignore-tests);
  values(quiet?, profiling?, start-suite, options, report-function)
end method compute-application-options;

define method run-test-application
    (parent :: <component>, 
     #key command-name = application-name(),
          arguments = application-arguments(),
          report-format-function = *format-function*)
 => (result :: <result>)
  // Process the command line arguments
  if (arguments & ~empty?(arguments))
    let (first-argument, keyword?) = process-argument(arguments[0]);
    if (keyword?
        & member?(first-argument, #["help", "?"], test: \=))
      help-function(command-name)
    end if;
  end if;
  let (quiet?, profiling?, start-suite, options, report-function) 
    = compute-application-options(parent, arguments);

  // Run the appropriate test or suite
  block ()
    unless (quiet?)
      display-run-options(start-suite, report-function, options)
    end;
    let result = #f;
    let handler <warning>
      = method (warning :: <warning>, next-handler :: <function>) => ()
          report-format-function("Warning: %s\n", warning);
          next-handler()
        end;
    profiling (cpu-time-seconds, cpu-time-microseconds, allocation)
      result := perform-component(start-suite, options, report-function: #f);
    results
      display-results(result,
		      report-function: report-function,
		      report-format-function: report-format-function);
      if (profiling?)
	format-out("\nTest run took %d.%s seconds, allocating %d byte%s\n",
		   cpu-time-seconds,
                   integer-to-string(cpu-time-microseconds, size: 6),
                   allocation, plural(allocation));
      end if;
      format-out("\n");
    end profiling;
    result
  afterwards
    end-test();
  end block;
end method run-test-application;

define not-inline function end-test ()
  // This function isn't intended to do anything; it just provides a place
  // to set a breakpoint before the program terminates.
  values()
end function end-test;
