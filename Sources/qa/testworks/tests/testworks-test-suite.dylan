Module:       testworks-test-suite
Summary:      A test suite to test the testworks harness
Author:       Andy Armstrong, James Kirsch, Shri Amit
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some utilities for testing TestWorks

define macro with-debugging
  { with-debugging () ?body:body end }
    => { let old-debug? = *debug?*;
	 block ()
	   *debug?* := #t;
	   ?body
	 cleanup
	   *debug?* := old-debug?;
	 end }
end macro with-debugging;

define macro without-recording
  { without-recording () ?body:body end }
    => { let old-check-recording-function = *check-recording-function*;
	 block ()
	   *check-recording-function* := always(#t);
	   ?body
	 cleanup
	   *check-recording-function* := old-check-recording-function
	 end }
end macro without-recording;

define class <test-warning> (<simple-warning>)
end class <test-warning>;

define function test-warning ()
  signal(make(<test-warning>, format-string: "internal test warning"))
end function test-warning;

define class <test-error> (<simple-error>)
end class <test-error>;

define function test-error ()
  error(make(<test-error>, format-string: "internal test error"))
end function test-error;


/// Check macros testing

define constant $internal-check-name = "Internal check";

define test testworks-check-test ()
  check("check(always(#t))", always(#t));
  check("check(identity, #t)", identity, #t);
  check("check(\\=, 3, 3)", \=, 3, 3);
end test testworks-check-test;

define test testworks-check-true-test ()
  check-equal("check-true(#t) passes",
	      without-recording ()
		check-true($internal-check-name, #t)
	      end,
              #"passed");
  check-equal("check-true(#f) fails",
	      without-recording ()
		check-true($internal-check-name, #f)
	      end,
              #"failed");
  check-true("check-true of error crashes",
	     instance?(without-recording ()
			 check-true($internal-check-name, 
				    test-error())
		       end,
	               <test-error>))
end test testworks-check-true-test;

define test testworks-check-false-test ()
  check-equal("check-false(#t) fails",
	      without-recording ()
		check-false($internal-check-name, #t)
	      end,
              #"failed");
  check-equal("check-false(#f) passes",
	      without-recording ()
		check-false($internal-check-name, #f)
	      end,
              #"passed");
  check-true("check-false of error crashes",
	     instance?(without-recording ()
			 check-false($internal-check-name, 
				     test-error())
		       end,
	               <test-error>))
end test testworks-check-false-test;

define test testworks-check-equal-test ()
  check-equal("check-equal(1, 1) passes",
	      without-recording ()
		check-equal($internal-check-name, 1, 1)
	      end,
              #"passed");
  check-equal("check-equal(\"1\", \"1\") passes",
	      without-recording ()
		check-equal($internal-check-name, "1", "1")
	      end,
              #"passed");
  check-equal("check-equal(1, 2) fails",
	      without-recording ()
		check-equal($internal-check-name, 1, 2)
	      end,
              #"failed");
  check-true("check-equal of error crashes",
	     instance?(without-recording ()
			 check-equal($internal-check-name, 
				     1,
				     test-error())
		       end,
	               <test-error>))
end test testworks-check-equal-test;

define test testworks-check-instance?-test ()
  check-equal("check-instance?(1, <integer>) passes",
	      without-recording ()
		check-instance?($internal-check-name, <integer>, 1)
	      end,
              #"passed");
  check-equal("check-instance?(1, <string>) fails",
	      without-recording ()
		check-instance?($internal-check-name, <string>, 1)
	      end,
              #"failed");
  check-true("check-instance? of error crashes",
	     instance?(without-recording ()
			 check-instance?($internal-check-name, 
					 <integer>,
					 test-error())
		       end,
	               <test-error>))
end test testworks-check-instance?-test;

define test testworks-check-condition-test ()
  let success? = #f;
  check-equal("check-condition catches <error>",
	      without-recording ()
		check-condition($internal-check-name,
				<test-error>,
				begin
				  test-warning();
				  success? := #t;
				  test-error()
				end)
	      end,
              #"passed");
  check-true("check-condition for <error> doesn't catch <warning>", success?);
  check-equal("check-condition fails if no condition",
	      without-recording ()
		check-condition($internal-check-name,
				<test-error>,
				#f)
	      end,
              #"failed");
  check-condition("check-condition doesn't catch wrong condition",
		  <warning>,
		  without-recording ()
		    check-condition($internal-check-name,
				    <test-error>,
				    test-warning())
		  end);
end test testworks-check-condition-test;

define test testworks-check-no-errors-test ()
  check-equal("check-no-errors of #t passes",
	      without-recording ()
		check-no-errors($internal-check-name, #t)
	      end,
              #"passed");
  check-equal("check-no-errors of #f passes",
	      without-recording ()
		check-no-errors($internal-check-name, #f)
	      end,
              #"passed");
  check-true("check-no-errors of error crashes",
	     instance?(without-recording ()
			 check-no-errors($internal-check-name, 
					 test-error())
		       end,
	               <test-error>))
end test testworks-check-no-errors-test;

define suite testworks-check-macros-suite ()
  test testworks-check-test;
  test testworks-check-true-test;
  test testworks-check-false-test;
  test testworks-check-equal-test;
  test testworks-check-instance?-test;
  test testworks-check-condition-test;
  test testworks-check-no-errors-test;
end suite testworks-check-macros-suite;


/// Verify the result objects

define test testworks-perform-test-results-test ()
  let test-to-check = testworks-check-test;
  let test-results
    = perform-test(test-to-check, progress-function: #f, report-function: #f);
  check-true("perform-test returns <test-result>", 
             instance?(test-results, <test-result>));
  check-equal("perform-test returns #\"passed\" when passing", 
              test-results.result-status, #"passed");
  check-true("perform-test sub-results are in a vector", 
	     instance?(test-results.result-subresults, <vector>))
end test testworks-perform-test-results-test;

define test testworks-perform-suite-results-test ()
  let suite-to-check = testworks-check-macros-suite;
  let suite-results
    = perform-suite(suite-to-check, progress-function: #f, report-function: #f);
  check-true("perform-suite returns <suite-result>", 
             instance?(suite-results, <suite-result>));
  check-equal("perform-suite returns #\"passed\" when passing", 
              suite-results.result-status, #"passed");
  check-true("perform-suite sub-results are in a vector", 
	     instance?(suite-results.result-subresults, <vector>))
end test testworks-perform-suite-results-test;

define suite testworks-results-suite ()
  test testworks-perform-test-results-test;
  test testworks-perform-suite-results-test;
end suite testworks-results-suite;


/// The master suite

define suite testworks-test-suite ()
  suite testworks-check-macros-suite;
  suite testworks-results-suite;
end suite testworks-test-suite;
