Module:       test-report
Synopsis:     A tool to generate reports from test run logs
Author:       Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Report functions:
//   full - report identical as well as different result objects
//   diff - reports only different result objects
//   summary - reports only a summary of passes, failures, etc.
//   benchmark - reports benchmarks that differed by more than a certain amount.

define method print-status-line
    (result :: <comparison-result>, #key indent = "", test)
 => ()
  let show-result? = if (test) test(result) else #t end;
  if (show-result?)
    let result1 = result.comparison-result1;
    let result2 = result.comparison-result2;
    format-out("%s%s %s -- %s\n",
               indent, 
	       result-type-name(result1 | result2),
	       result-name(result1 | result2),
	       if (result.comparison-identical?)
		 "identical" 
	       else 
		 "differed" 
	       end)
  end if;
end method print-status-line;

define method result-failure-reason
    (result :: <check-result>) => (reason :: false-or(<string>))
  failure-reason
    (result-status(result), result-operation(result), result-value(result))
end method result-failure-reason;

define method result-failure-reason
    (result :: <test-result>) => (reason :: false-or(<string>))
  let status = result-status(result);
  instance?(status, <error>) & safe-error-to-string(status)
end method result-failure-reason;

define method print-result-reason
    (name :: <string>, result :: <result>, #key indent = "") => ()
  let reason = result-failure-reason(result);
  format-out("%s  %s %s%s\n",
	     indent, name, status-name(result.result-status),
	     if (reason) format-to-string(" [%s]", reason) else "" end);
end method print-result-reason;

define method print-result-reason
    (name :: <string>, result :: <benchmark-result>, #key indent = "") => ()
  if (result.result-status ~== #"passed")
    next-method();
  else
    format-out("%s  %s %s in %s seconds, %d bytes allocated\n",
               indent, name, status-name(result-status(result)),
               result-time(result), result-bytes(result))
  end if
end method print-result-reason;

define method print-result-reason
    (name :: <string>, result == #f, #key indent = "") => ()
  format-out("%s  %s has no such match\n", indent, name);
end method print-result-reason;

define method print-reason
    (result :: <comparison-result>, #key indent = "", test)
 => ()
  let show-result? = if (test) test(result) else #t end;
  if (show-result?)
    print-comparison-result(result.comparison-result1, result.comparison-result2,
                            indent: indent);
  end;
end method print-reason;

define method print-comparison-result
    (result1, result2, #key indent = "") => ()
  print-result-reason("Log1", result1, indent: indent);
  print-result-reason("Log2", result2, indent: indent);
end method print-comparison-result;

define method float->percentage
    (f :: <float>) => (s :: <string>)
  let sign = if (f < 0) "-" else "+" end;
  let (int1, ff) = floor(abs(f));
  let int2 = abs(round(ff * 100));
  if (int1 == 0 & int2 == 0)
    sign := "";
  end if;
  format-to-string("%s%d.%s%%", sign, int1, integer-to-string(int2, size: 2))
end method float->percentage;

// This method is provided only for the case where checks and benchmarks are
// mixed together in the same test or suite.  A much better way to display
// benchmark results is to use the benchmark-diff-report-function.
//
define method print-comparison-result
    (result1 :: <benchmark-result>, result2 :: <benchmark-result>,
     #key indent = "")
 => ()
  next-method();
  let (time-diff, space-diff, valid?) = diff-benchmark-results(result1, result2);
  if (valid?)
    format-out("%s   ==> time: %s%%, allocation: %s%%\n",
               indent,
               float->percentage(time-diff),
               float->percentage(space-diff));
  else
    format-out("%s  ==> *** benchmark results invalid ***\n", indent);
  end if;
end method print-comparison-result;

define method print-comparison-info
    (result :: <comparison-result>, #key indent = "", test)
 => ()
  print-status-line(result, indent: indent, test: test);
  let result1 = result.comparison-result1;
  let result2 = result.comparison-result2;
  if (instance?(result1 | result2, <unit-result>))
    print-reason(result, indent: indent, test: test);
  end;
  let subindent = concatenate-as(<byte-string>, indent, "  ");
  for (subresult in comparison-subresults(result))
    print-comparison-info(subresult, indent: subindent, test: test)
  end
end method print-comparison-info;

define method print-percentage 
    (count :: <integer>, size :: <integer>, #key decimal-places = 1)
 => ()
  case
    size > 0 =>
      let shift = 10; // 10 ^ decimal-places;
      let percentage = ceiling/(count * 100 * shift, size);
      let (integer, remainder) = floor/(percentage, shift);
      format-out("%d.%d%%", integer, floor(remainder));
    otherwise =>
      format-out("100%%");
  end
end method print-percentage;

define method count-results
    (result :: <comparison-result>, type :: <string>)
 => (differed :: <integer>, identical :: <integer>, unique :: <integer>)
  let differed  = 0;
  let identical = 0;
  let unique    = 0;
  
  let result1 = result.comparison-result1;
  let result2 = result.comparison-result2;
  let comparison-type = result-type-name(result1 | result2);
  if (comparison-type = type)
    if (result.comparison-identical?)
      identical := identical + 1;
    else
      differed  := differed + 1;
    end;
    unless (result1 & result2)
      unique := unique + 1;
    end;
  end;
  for (subresult in result.comparison-subresults)
    let (sub-differed, sub-identical, sub-unique)
      = count-results(subresult, type);
    differed  := differed  + sub-differed;
    identical := identical + sub-identical;
    unique    := unique    + sub-unique;
  end;
  values(differed, identical, unique)
end method count-results;
  
define method summarize
    (result :: <comparison-result>, type :: <string>)
 => ()
  let (differed, identical, unique)
     = count-results(result, type);
  let total = differed + identical + unique;
  format-out("   Compared %d %ss: %d differed(",
	     total, type, differed);
  print-percentage(differed, total);
  format-out("), %d identical and %d unique\n",
	     identical, unique);
end method summarize;


/// Report functions

define method diff-full-report-function 
    (result :: <comparison-result>) => ()
  print-comparison-info(result, test: always(#t));
  format-out("\n");
  diff-summary-report-function(result)
end method diff-full-report-function;

define method contains-check-results?
    (result :: <comparison-result>) => (b :: <boolean>)
  instance?(comparison-result1(result), <check-result>)
  | instance?(comparison-result2(result), <check-result>)
  | any?(contains-check-results?, comparison-subresults(result))
end method contains-check-results?;

define method diff-report-function
    (result :: <comparison-result>) => ()
  // To maintain backward compatibility with pre-benchmarking code, if the
  // comparison contains any checks then use the non-benchmark display
  // format.
  if (contains-check-results?(result))
    print-comparison-info(result,
                          test: method (result)
                                  ~result.comparison-identical?
                                end);
  else
    benchmark-diff-report-function(result, show-all?: #f);
  end if;
  format-out("\n");
  diff-summary-report-function(result)
end method diff-report-function;

define method diff-summary-report-function
    (result :: <comparison-result>) => ()
  format-out("Comparison Summary:\n");
  summarize(result, "Suite");
  summarize(result, "Test");
  summarize(result, "Check");
  summarize(result, "Benchmark");
end method diff-summary-report-function;

define method benchmark-diff-report-function
    (top-result :: <comparison-result>, #key show-all? :: <boolean>)
 => ()
  let any-displayed? = #f;
  let crashes = 0;
  let best-time-diff = 0;
  let worst-time-diff = 0;
  let best-alloc-diff = 0;
  let worst-alloc-diff = 0;
  local method print-one (result :: <comparison-result>) => ()
          let result1 = comparison-result1(result);
          let result2 = comparison-result2(result);
          if (instance?(result1, <benchmark-result>)
              & instance?(result2, <benchmark-result>)
              & (~comparison-identical?(result) | show-all?))
            let (time-diff, space-diff, valid?)
              = diff-benchmark-results(result1, result2);
            if (~any-displayed?)
              any-displayed? := #t;
              print-benchmark-result-header();
            end if;
            print-one-benchmark-result
              (result-name(result1),
               if (valid?) float->percentage(time-diff) else "N/A" end,
               if (valid?) float->percentage(space-diff) else "N/A" end);
            if (valid?)
              best-time-diff := min(best-time-diff, time-diff);
              worst-time-diff := max(worst-time-diff, time-diff);
              best-alloc-diff := min(best-alloc-diff, space-diff);
              worst-alloc-diff := max(worst-alloc-diff, space-diff);
            else
              crashes := crashes + 1;
            end if;
          end;
          for (subresult in comparison-subresults(result))
            print-one(subresult)
          end;
        end method;
  print-one(top-result);
  if (any-displayed?)
    print-benchmark-result-footer("Worst regression:",
                                  float->percentage(best-time-diff),
                                  float->percentage(best-alloc-diff),
                                  0);
    print-benchmark-result-footer("Best improvement:",
                                  float->percentage(worst-time-diff),
                                  float->percentage(worst-alloc-diff),
                                  crashes, divider?: #f);
  else
    format-out("*** No benchmark results differed by more than %d%%.\n",
               *benchmark-tolerance*);
  end if;
end method benchmark-diff-report-function;

