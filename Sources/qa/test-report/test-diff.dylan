Module:       test-report
Synopsis:     A tool to generate reports from test run logs
Author:       Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $default-benchmark-tolerance :: <integer>
  = 2;  // i.e., 2% change from comparison benchmark

define thread variable *benchmark-tolerance* :: <integer>
  = $default-benchmark-tolerance;

/// Comparison result class

define class <comparison-result> (<object>)
  constant slot comparison-identical? :: <boolean>,
    required-init-keyword: identical?:;
  constant slot comparison-result1 :: false-or(<result>),
    required-init-keyword: result1:;
  constant slot comparison-result2 :: false-or(<result>),
    required-init-keyword: result2:;
  constant slot comparison-subresults :: <sequence> = make(<stretchy-vector>),
    init-keyword: subresults:;
end class <comparison-result>;


/// Comparison of two test run logs

define method equivalent
    (result1 :: false-or(<result>), result2 :: false-or(<result>))
 => (boolean :: <boolean>)
  result1
  & result2
  & object-class(result1) = object-class(result2)
  & result1 = result2
end method equivalent;

define sideways method \=
    (result1 :: <benchmark-result>, result2 :: <benchmark-result>)
 => (equal? :: <boolean>)
  next-method()
  & begin
      let (time-diff, space-diff, valid?)
        = diff-benchmark-results(result1, result2);
      // Note that the use of < here rather than <= is mostly so that a tolerance
      // of 0 will show all benchmark results, even those that haven't changed at all.
      abs(time-diff) < *benchmark-tolerance*
      & abs(space-diff) < *benchmark-tolerance*
    end
end method \=;

define method diff-benchmark-results
    (result1 :: <benchmark-result>, result2 :: <benchmark-result>)
 => (time-diff :: <float>, space-diff :: <float>, valid? :: <boolean>)
  let sec1 = result1.result-seconds;
  let sec2 = result2.result-seconds;
  let usec1 = result1.result-microseconds;
  let usec2 = result2.result-microseconds;
  let bytes1 = result1.result-bytes;
  let bytes2 = result2.result-bytes;
  if (sec1 & sec2 & usec1 & usec2 & bytes1 & bytes2)
    values(percent-change(1e6 * sec1 + usec1, 1e6 * sec2 + usec2),
           percent-change(as(<float>, bytes1), as(<float>, bytes2)),
           #t)
  else
    // +++ Note that if all slot values were #f we get here.  Technically I
    // suppose the benchmarks are the same if that's the case...
    values(0.0, 0.0, #f)
  end if
end method diff-benchmark-results;

// Returns b as a relative percentage of a.  For example, if a = 9 and b = 10,
// then it returns 10 because b is 10% more than a.  If a = 10 and b = 9 then
// it returns -11 because b is 11% less than a.
define function percent-change
    (a :: <float>, b :: <float>) => (percent-change :: <float>, )
  100 * (1 - a / b)
end;

define method result<
    (result1 :: <result>, result2 :: <result>) => (less-than? :: <boolean>)
  result1.result-name < result2.result-name
end method result<;

define method compare-subresults
    (subresults1 :: <sequence>, subresults2 :: <sequence>)
 => (comp-results :: <sequence>)
  let subresults1 = sort(subresults1, test: result<);
  let subresults2 = sort(subresults2, test: result<);
  let comp-results = make(<stretchy-vector>);
  let size1        = subresults1.size;
  let size2        = subresults2.size;
  let index1 :: <integer> = 0;
  let index2 :: <integer> = 0;
  
  while (index1 < size1 & index2 < size2)
    let subresult1 :: <result> = subresults1[index1];
    let subresult2 :: <result> = subresults2[index2];
    let name1 = subresult1.result-name;
    let name2 = subresult2.result-name;
    case
      name1 = name2 =>
	add!(comp-results, create-comparison-result(subresult1, subresult2));
	index1 := index1 + 1;
	index2 := index2 + 1;
      name1 < name2 =>
	add!(comp-results, create-comparison-result(subresult1, #f));
	index1 := index1 + 1;
      otherwise =>
	add!(comp-results, create-comparison-result(#f, subresult2));
	index2 := index2 + 1;
    end;
  end;

  for (index from index1 below size1)
    add!(comp-results, create-comparison-result(subresults1[index], #f))
  end;
  for (index from index2 below size2)
    add!(comp-results, create-comparison-result(#f, subresults2[index]))
  end;

  comp-results
end method compare-subresults;


define method compare-results
    (result1 :: false-or(<result>), result2 :: false-or(<result>))
 => (identical? :: <boolean>, subresults :: <sequence>)
  let result1-component? = instance?(result1, <component-result>);
  let result2-component? = instance?(result2, <component-result>);
  let subresults
    = case
	result1-component? & result2-component? =>
	  compare-subresults(result1.result-subresults, result2.result-subresults);
	result1-component? =>
	  compare-subresults(result1.result-subresults, #[]);
	result2-component? =>
	  compare-subresults(#[], result2.result-subresults);
	otherwise =>
	  #[]
      end;
  let identical?
    = every?(method (subresult)
	       subresult.comparison-identical?
	     end,
	     subresults)
        & equivalent(result1, result2);
  values(identical?, subresults)
end method compare-results;

define method create-comparison-result
    (result1 :: false-or(<result>), result2 :: false-or(<result>))
 => (comp-result :: <comparison-result>)
  let (identical?, subresults) = compare-results(result1, result2);
  make(<comparison-result>,
       result1: result1, result2: result2,
       identical?: identical?, subresults: subresults)
end method create-comparison-result;

define method create-comparison-result
    (path1 :: <string>, path2 :: <string>)
 => (comp-result :: <comparison-result>)
  let result1 = read-log-file(path1);
  let result2 = read-log-file(path2);
  create-comparison-result(result1, result2)
end method create-comparison-result;


/// Test diff

define class <log-comparison-result> (<comparison-result>)
end class <log-comparison-result>;

define method perform-test-diff
    (#key log1, log2, result1, result2, report-function = diff-report-function,
          tolerance :: <integer> = $default-benchmark-tolerance)
 => ()
  let result1 = result1 | read-log-file(log1);
  let result2 = result2 | read-log-file(log2);
  dynamic-bind(*benchmark-tolerance* = tolerance)
    let (identical?, subresults) = compare-results(result1, result2);
    let result = make(<log-comparison-result>,
                      result1: result1, 
                      result2: result2,
                      identical?: identical?,
                      subresults: subresults);
    report-function & report-function(result);
  end;
end method perform-test-diff;

