Module:       test-report
Synopsis:     A tool to generate reports from test run logs
Author:	      Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Some application exit constants

define table $error-codes
  = { #"help"                          => 1,
      #"bad-argument-value"            => 2,
      #"start-token-not-found"         => 3,
      #"end-token-not-found"           => 4,
      #"token-not-found"               => 5,
      #"invalid-report-function"       => 6,
      #"invalid-command-line-argument" => 7,
      #"missing-log-file"              => 8,
      #"no-matching-results"           => 9,
      #"file-not-found"                => 10,
      #"end-of-file"                   => 11
    };


/// Application options

define class <application-options> (<object>)
  constant slot application-quiet? :: <boolean> = #f,
    init-keyword: quiet?:;
  constant slot application-log1 :: <string>,
    required-init-keyword: log1:;
  constant slot application-log2 :: false-or(<string>) = #f,
    init-keyword: log2:;
  constant slot application-report-function :: <function>,
    required-init-keyword: report-function:;
  constant slot application-tests :: <sequence> = #[],
    init-keyword: tests:;
  constant slot application-suites :: <sequence> = #[],
    init-keyword: suites:;
  constant slot application-ignored-tests :: <sequence> = #[],
    init-keyword: ignored-tests:;
  constant slot application-ignored-suites :: <sequence> = #[],
    init-keyword: ignored-suites:;
  constant slot application-tolerance :: <integer> = $default-benchmark-tolerance,
    init-keyword: tolerance:;
end class <application-options>;

define function print-elements
    (sequence :: <sequence>, #key prefix = "", postfix = "\n") => ()
  let separator = ", ";
  let current-separator = "";
  let sequence-size = size(sequence);
  format-out(prefix);
  for (element in sequence)
    format-out("%s%s", current-separator, element);
    current-separator := separator
  end;
  format-out(postfix);
end function print-elements;

define method display-run-options
    (options :: <application-options>) => ()
  unless (application-quiet?(options))
    let log1 = application-log1(options);
    let log2 = application-log2(options);
    let report-function = application-report-function(options);
    let tests = application-tests(options);
    let suites = application-suites(options);
    let ignored-tests = application-ignored-tests(options);
    let ignored-suites = application-ignored-suites(options);
    format-out("\n");
    if (log2)
      format-out("Comparing log files:\n  %s\n  %s\n\n", log1, log2)
    else
      format-out("Generating report for:\n  %s\n", log1)
    end;
    format-out("\n    Report function: %s\n",
	       select (report-function by \=)
		 diff-full-report-function    => "full-diff";
		 diff-report-function         => "diff";
		 diff-summary-report-function => "diff-summary";
                 benchmark-diff-report-function => "benchmark-diff";
		 summary-report-function      => "summary";
		 failures-report-function     => "failures";
		 full-report-function         => "full";
		 otherwise                    => "*** unrecognised ***";
	       end);
    print-elements(tests,          prefix: "              Tests: ");
    print-elements(suites,         prefix: "             Suites: ");
    print-elements(ignored-tests,  prefix: "      Ignored Tests: ");
    print-elements(ignored-suites, prefix: "     Ignored Suites: ");
    format-out("Benchmark tolerance: %d%%\n", application-tolerance(options));
    format-out("\n")
  end
end method display-run-options;

/// application-error

define method application-exit-code
    (error-name :: <symbol>) => (code :: <integer>)
  let error-code = element($error-codes, error-name, default: #f);
  error-code | error("Unknown error value '%='", error-name)
end method application-exit-code;


/// Command line arguments

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
  & block ()   // don't treat a negative integer as a keyword arg.
      let (int, index) = string-to-integer(argument);
      ignore(int);
      size(argument) ~= index
    exception (e :: <error>)
      #t
    end
end method keyword-argument?;

define method invalid-argument
    (error-name :: <symbol>, format-string :: <string>, #rest args) => ()
  display-help(application-name());
  format-out("\n");
  apply(format-out, format-string, args);
  exit-application(application-exit-code(error-name))
end method invalid-argument;

define method argument-value
    (keyword :: <string>, arguments :: <deque>,
     #key allow-zero-arguments?)
 => (value :: <stretchy-vector>)
  if (~allow-zero-arguments?
      & (empty?(arguments) | keyword-argument?(arguments[0])))
    invalid-argument(#"bad-argument-value",
		     "No argument specified for keyword '%s'.\n", keyword)
  end;
  let value = make(<stretchy-vector>);
  while (~empty?(arguments) & ~keyword-argument?(arguments[0]))
    add!(value, pop(arguments))
  end;
  value
end method argument-value;

// ---*** carlg 99-02-12 I think it would be a good idea to change the arguments
//        as follows:
//          -report [all | failures | summary]
//        and the rest can be figured out based on whether one or two log files
//        were specified.  No time now though...
define constant $help-format-string =
  "Application: %s\n"
  "\n"
  "  Arguments: log1\n"
  "             [log2]\n"
  "             [-quiet]\n"
  "             [-report [full failures summary diff full-diff diff-summary benchmark-diff]]\n"
  "             [-suite <name1> <name2> ... ...]\n"
  "             [-test <name1> <name2> ... ...]\n"
  "             [-ignore-suite <name1> <name2> ... ...]\n"
  "             [-ignore-test <name1> <name2> ... ...]\n"
  "             [-tolerance <percentage>]\n";

define method display-help (command-name :: <string>) => ()
  format-out($help-format-string, command-name);
end method display-help;

define method parse-arguments
    (command-name :: <sequence>, arguments :: <sequence>)
 => (options :: <application-options>)
  let arguments = as(<deque>, arguments);
  let log1 = #f;
  let log2 = #f;
  let suites = #[];
  let tests = #[];
  let ignored-suites = #[];
  let ignored-tests = #[];
  let report-function = #f;
  let quiet? = #f;
  let tolerance = $default-benchmark-tolerance;
  // Parse through the arguments
  while (~empty?(arguments))
    let argument = pop(arguments);
    let (option, keyword?) = process-argument(argument);
    select (option by \=)
      "report" =>
	report-function
	  := begin
	       let function-name = pop(arguments);
	       select (function-name by \=)
		 "full"         => full-report-function;
		 "summary"      => summary-report-function;
		 "failures"     => failures-report-function;
		 "diff"         => diff-report-function;
		 "full-diff"    => diff-full-report-function;
		 "diff-summary" => diff-summary-report-function;
                 "benchmark-diff" => benchmark-diff-report-function;
		 otherwise =>
                   invalid-argument(#"invalid-report-function",
				    "Report function '%s' not supported.\n",
                                     function-name);
	       end
	     end;
      "suite" =>
	suites := concatenate(suites, argument-value(option, arguments));
      "test" =>
	tests := concatenate(tests, argument-value(option, arguments));
      "ignore-suite" =>
	ignored-suites 
	  := concatenate(ignored-suites, argument-value(option, arguments));
      "ignore-test" =>
	ignored-tests 
	  := concatenate(ignored-tests,  argument-value(option, arguments));
      "quiet" =>
	quiet? := #t;
      "verbose" =>
	quiet? := #f;
      "tolerance" =>
        let vals = argument-value(option, arguments);
        block ()
          tolerance := string-to-integer(vals[0]);
        exception (e :: <error>)
          invalid-argument(#"bad-argument-value",
                           "Invalid argument specified for the %s keyword: '%s'.\n",
                           option, vals[0]);
        end;
      otherwise =>
        case
          log1 & log2 =>
	    invalid-argument(#"invalid-command-line-argument",
			     "Invalid command line keyword '%s'.\n", option);
          log1      => log2 := option;
          otherwise => log1 := option;
        end;
    end
  end;
  unless (log1)
    invalid-argument(#"missing-log-file",
		     "Log file missing - one or two log files must be supplied\n")
  end;
  unless (report-function)
    report-function := if (log2)
                         diff-report-function
                       else
                         failures-report-function
                       end;
  end;
  if (log2 & member?(report-function, vector(full-report-function,
                                             failures-report-function,
                                             summary-report-function)))
    invalid-argument(#"bad-argument-value",
                     "The report function specified is not meaningful "
                     "when two log files are specified.\n");
  end if;
  if (~log2 & member?(report-function, vector(diff-report-function,
                                              diff-full-report-function,
                                              diff-summary-report-function,
                                              benchmark-diff-report-function)))
    invalid-argument(#"bad-argument-value",
                     "The report function specified is only meaningful "
                     "when two log files are specified.\n");
  end if;
  make(<application-options>,
       log1: log1, log2: log2,
       quiet?: quiet?, report-function: report-function,
       tolerance: tolerance,
       tests: tests, suites: suites,
       ignored-tests:  map(as-lowercase, ignored-tests), 
       ignored-suites: map(as-lowercase, ignored-suites))
end method parse-arguments;

define method case-insensitive-equal?
    (name1 :: <string>, name2 :: <string>)
 => (equal? :: <boolean>)
  as-lowercase(name1) = as-lowercase(name2)
end method case-insensitive-equal?;

define method find-named-results
    (result :: <check-result>, #key tests = #[], suites = #[])
 => (named-results :: <sequence>)
  #[]
end method find-named-results;

define method find-named-results
    (results :: <sequence>, #key tests = #[], suites = #[])
 => (named-results :: <sequence>)
  let named-results = make(<stretchy-vector>);
  for (subresult in results)
    let subresults
      = find-named-results(subresult, tests: tests, suites: suites);
    for (result in subresults)
      add!(named-results, result)
    end
  end;
  named-results
end method find-named-results;

define method find-named-results
    (result :: <test-result>, #key tests = #[], suites = #[])
 => (named-results :: <sequence>)
  let match?
    = member?(result.result-name, tests, test: case-insensitive-equal?);
  if (match?)
    vector(result)
  else
    find-named-results
      (result.result-subresults, tests: tests, suites: suites)
  end
end method find-named-results;

define method find-named-results
    (result :: <suite-result>, #key tests = #[], suites = #[])
 => (named-results :: <sequence>)
  let match?
    = member?(result.result-name, suites, test: case-insensitive-equal?);
  if (match?)
    vector(result)
  else
    find-named-results
      (result.result-subresults, tests: tests, suites: suites)
  end
end method find-named-results;

define method find-named-result
    (result :: <result>, #key tests = #[], suites = #[])
 => (named-result :: <result>)
  let results = find-named-results(result, tests: tests, suites: suites);
  select (size(results))
    0 =>
      application-error(#"no-matching-results",
			"No matches for tests %= or suites %=",
			tests, suites);
    1 =>
      results[0];
    otherwise =>
      let passed?
	= every?(method (subresult)
		   let status = subresult.result-status;
		   status = #"passed" | status = #"not-executed"
		 end,
		 results);
      make(<suite-result>,
	   name: "[Specified tests/suites]",
	   status: if (passed?) #"passed" else #"failed" end,
	   subresults: results);
  end
end method find-named-result;

define method main
    (command-name :: <string>, arguments :: <sequence>) => ()
  // Process the command line arguments
  if (arguments & ~empty?(arguments))
    let (first-argument, keyword?) = process-argument(arguments[0]);
    if (keyword?
 	& member?(first-argument, #["help", "?"], test: \=))
      display-help(command-name);
      exit-application(application-exit-code(#"help"));
    end if;
  end if;
  let options = parse-arguments(command-name, arguments);
  display-run-options(options);
  let log1 = application-log1(options);
  let log2 = application-log2(options);
  let tests = application-tests(options);
  let suites = application-suites(options);
  let report-function = application-report-function(options);
  let ignored-tests = application-ignored-tests(options);
  let ignored-suites = application-ignored-suites(options);
  let tolerance = application-tolerance(options);
  local method read-log-file-with-options
            (log :: <string>) => (result :: <result>)
          block ()
            let result
              = read-log-file(log, 
                              ignored-tests: ignored-tests, 
  			      ignored-suites: ignored-suites);
            if (~empty?(tests) | ~empty?(suites))
  	      find-named-result(result,
                                tests: tests,
                                suites: suites)
  	    else
  	      result
  	    end
          exception (e :: <file-does-not-exist-error>)
            application-error(#"file-not-found", "Error: %s", e);
          end block
        end method read-log-file-with-options;
  if (log2)
    let result1 = read-log-file-with-options(log1);
    let result2 = read-log-file-with-options(log2);
    perform-test-diff
      (log1: log1, log2: log2,
       result1: result1, result2: result2,
       report-function: report-function,
       tolerance: tolerance)
  else
    let results = read-log-file-with-options(log1);
    report-function(results)
  end
end method main;

