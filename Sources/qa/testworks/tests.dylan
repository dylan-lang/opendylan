Module:       testworks
Summary:      Testworks harness
Author:       Andrew Armstrong, James Kirsch
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Tests

define class <test> (<component>)
  constant slot test-function :: <function>, 
    required-init-keyword: function:;
end class <test>;

define constant $test-objects-table = make(<table>);

define method find-test-object
    (function :: <function>) => (test :: false-or(<test>))
  element($test-objects-table, function, default: #f)
end method find-test-object;

define class <unit-result> (<result>)
  constant slot result-operation :: <check-operation-type>,
    required-init-keyword: operation:;
  constant slot result-value :: <check-value-type>,
    required-init-keyword: value:;
end class <unit-result>;

define class <check-result> (<unit-result>)
end;

define method result-type-name
    (result :: <check-result>) => (name :: <string>)
  "Check"
end;

define class <benchmark-result> (<unit-result>)
  constant slot result-seconds :: false-or(<integer>),
    required-init-keyword: seconds:;
  constant slot result-microseconds :: false-or(<integer>),
    required-init-keyword: microseconds:;
  // Hopefully no benchmarks will allocated more than 536MB...
  constant slot result-bytes :: false-or(<integer>),
    required-init-keyword: bytes:;
end;

define method result-type-name
    (result :: <benchmark-result>) => (name :: <string>)
  "Benchmark"
end;

define method result-time
    (result :: <benchmark-result>, #key pad-seconds-to :: false-or(<integer>))
 => (seconds :: <string>)
  time-to-string(result-seconds(result), result-microseconds(result),
                 pad-seconds-to: pad-seconds-to)
end method result-time;

// the test macro

//---*** We could use 'define function' but it doesn't debug as well right now
define macro test-definer
  { define test ?test-name:name (?keyword-args:*) ?test-body:body end }
    => { define method ?test-name () 
           ?test-body
         end method ?test-name;
         $test-objects-table[?test-name]
           := make(<test>,
                   name: ?"test-name",
                   function: ?test-name,
                   ?keyword-args); }
end macro test-definer;

// with-test-unit macro

//---*** Can we do something better than pretend this was a check?
define method record-test-unit-crash
    (name :: <string>, error :: <error>) => ()
  let unit-name :: <byte-string>
    = concatenate-as(<byte-string>, "Test unit ", name);
  record-check(unit-name, error, error, #f)
end method record-test-unit-crash;

define macro with-test-unit
  { with-test-unit (?name:expression, ?keyword-args:*) ?test-body:body end }
    => { begin
           let error = maybe-trap-errors(begin ?test-body; #f end);
           error & record-test-unit-crash(?name, error)
         end; }
end macro with-test-unit;

// perform-test takes a <test> object and returns a component-result object. 

define method perform-test
    (test :: <test>,
     #key tags                     = $all,
          announce-function        = #f,
          announce-checks?         = *announce-checks?*,
          progress-format-function = *format-function*,
          report-format-function   = *format-function*,
          progress-function        = *default-progress-function*,
          report-function          = *default-report-function*,
          debug?                   = *debug?*)
 => (result :: <component-result>)
  perform-component
    (test, 
     make(<perform-options>,
          tags:                     tags,
          announce-function:        announce-function,
          announce-checks?:         announce-checks?,
          progress-report-function: progress-format-function,
          progress-function:        progress-function | null-progress-function,
          debug?:                   debug?),
     report-function:        report-function | null-report-function,
     report-format-function: report-format-function);
end method perform-test;

define method perform-test
    (function :: <function>,
     #key tags                     = $all,
          announce-function        = #f,
          announce-checks?         = *announce-checks?*,
          progress-format-function = *format-function*,
          report-format-function   = *format-function*,
          progress-function        = *default-progress-function*,
          report-function          = *default-report-function*,
          debug?                   = *debug?*)
 => (result :: <component-result>)
  let test = find-test-object(function);
  if (test)
    perform-test(test, 
                 tags: tags,
                 announce-function:        announce-function,
                 announce-checks?:         announce-checks?,
                 progress-format-function: progress-format-function,
                 report-format-function:   report-format-function,
                 progress-function:        progress-function,
                 report-function:          report-function,
                 debug?:                   debug?)
  else
    error("Cannot perform-test on the non-test function %=", function)
  end
end method perform-test;

define method execute-component
    (test :: <test>, options :: <perform-options>)
 => (subresults :: <sequence>, status :: <result-status>,
     seconds, useconds, bytes)
  let subresults = make(<stretchy-vector>);
  let status :: <result-status>
    = dynamic-bind 
        (*debug?* = options.perform-debug?,
         *check-recording-function* =
           method (result :: <result>)
             add!(subresults, result);
             options.perform-progress-function(result);
             result
           end)
        let error = maybe-trap-errors(test.test-function());
        case
          instance?(error, <error>) =>
            error;
          every?(method (result :: <unit-result>) => (passed? :: <boolean>)
                   result.result-status == #"passed"
                 end, 
                 subresults) =>
            #"passed";
          otherwise =>
            #"failed"
        end
      end;
  values(subresults, status)
end method execute-component;

/// Some progress functions

define method null-progress-function
    (result :: <unit-result>) => ()
  #f
end method null-progress-function;

define method full-progress-function
    (result :: <unit-result>) => ()
  print-check-progress(result)
end method full-progress-function;

define variable *default-progress-function* = null-progress-function;


/// Some report functions

// This 'after' method prints the reason for the result's failure
define method print-result-info
    (result :: <unit-result>, #key indent = "", test) => ()
  ignore(indent);
  next-method();
  let show-result? = if (test) test(result) else #t end;
  if (show-result?)
    print-failure-reason(result.result-status, 
                         result.result-operation, 
                         result.result-value)
  end
end method print-result-info;
    
