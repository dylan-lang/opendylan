Module:       testworks
Synopsis:     Testworks benchmarks
Author:       Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Benchmarks

/// Note that <benchmark-result> is defined in tests.dylan

define macro benchmark
  { benchmark (?benchmark-name:expression, ?benchmark:expression)
  }
 =>
  { do-benchmark(method ()
                   ?benchmark-name
                 end,
                 method ()
                   vector(method ()
                            ?benchmark;
                            #t  // Benchmarks succeed unless they crash.  Reasonable???
                          end,
                          vector())
                 end)
  }
end macro benchmark;

// ---*** carlg 99-02-05 This shares a lot of code with do-check.  Might
//        want to try to combine them with a macro or something.
define method do-benchmark
    (name-function :: <function>, argument-function :: <function>) 
 => (status :: <result-status>)
  block ()
    let name = evaluate-name-function(name-function);
    let bench-arguments = maybe-trap-errors(argument-function());
    case
      instance?(name, <error>) =>
	record-benchmark("[*** Invalid name ***]", name, name, #f, #f, #f, #f);
      instance?(bench-arguments, <error>) =>
	record-benchmark(name, bench-arguments, bench-arguments, #f, #f, #f, #f);
      otherwise =>
	let function  = bench-arguments[0];
	let arguments = bench-arguments[1];
        let result = #f;
        let status = #f;
        profiling (cpu-time-seconds, cpu-time-microseconds, allocation)
  	  result := maybe-trap-errors(apply(function, arguments));
        results
          status := if (~result)
                      #"failed"
                    elseif (instance?(result, <error>))
                      result
                    else
                      #"passed"
                    end if;
          if (status == #"failed" & debug-failures?())
            break("Benchmark failed: %s", name)
          end if;
          record-benchmark(name, status, function, arguments,
                           cpu-time-seconds, cpu-time-microseconds, allocation);
        end;
        status
    end case;
  exception (r :: <simple-restart>,
	     init-arguments: vector(format-string:, "Skip this benchmark",
				    format-arguments:, #[]))
    #"failed"
  end block;
end method do-benchmark;

/// Benchmark recording

define method record-benchmark
    (name :: <string>,
     status :: <result-status>,
     operation :: <check-operation-type>,
     value :: <check-value-type>,
     seconds :: false-or(<integer>),
     microseconds :: false-or(<integer>),
     bytes-allocated :: false-or(<integer>))
 => (status :: <result-status>)
  let result = make(<benchmark-result>,
                    name: name, status: status, operation: operation, value: value,
                    seconds: seconds, microseconds: microseconds,
                    bytes: bytes-allocated);
  *check-recording-function*(result);
  status
end method record-benchmark;


/// A few utilities related to benchmarks

define function time-to-string
    (seconds :: false-or(<integer>), microseconds :: false-or(<integer>),
     #key pad-seconds-to :: false-or(<integer>))
 => (seconds :: <string>)
  if (seconds & microseconds)
    format-to-string("%s.%s",
                     integer-to-string(seconds, size: pad-seconds-to, fill: ' '),
                     integer-to-string(microseconds, size: 6))
  else
    "N/A"
  end
end;


// Add two times that are encoded as seconds + microseconds.
// Assumes the first time is valid.  The second time may be #f.
//
define method addtimes
    (sec1, usec1, sec2, usec2)
 => (sec, usec)
  if (sec2 & usec2)
    let sec = sec1 + sec2;
    let usec = usec1 + usec2;
    if (usec >= 1000000)
      usec := usec - 1000000;
      sec1 := sec1 + 1;
    end if;
    values(sec, usec)
  else
    values(sec1, sec2)
  end if
end method addtimes;
