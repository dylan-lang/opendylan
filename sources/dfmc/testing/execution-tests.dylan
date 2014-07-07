Module:    DFMC-Testing
Author:    Steve Rowley
Synopsis:  Tests for the typist's inference.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $trace-execution = #f;

define macro execution-test-definer
  { define execution-test ?testname:name
      ?test-code:expression => ?result:expression
    end }
  => {
    define test "execution-" ## ?testname ()
      let (ld, tlf*) = compile-to-top-level-forms(?test-code);
      with-library-context (ld)
        let transaction-id = #f;
        for (tlf in tlf*)
          transaction-id := interpret-top-level-form(tlf,
                                                     trace?: $trace-execution);
        end;
        format-out("transaction-id: %=\n", transaction-id);
        assert-equal(?result, interpreter-transaction-value(transaction-id), ?test-code);
      end;
    end }
end;

define function compile-to-top-level-forms (string :: <string>) => (ld, tlf*)
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let ld = compile-template(string, compiler: compile-library-until-optimized);
    let cr* = library-description-compilation-records(ld);
    // One for lib+mod defn & one for the source template.
    debug-assert(size(cr*) == 2, "Expected exactly 2 <compilation-record>s: %=", cr*);
    values(ld, compilation-record-top-level-forms(cr*[1]))
  end
end;

define execution-test basic
  "define no-inline method f () 1 end; f()" => 1
end;

define execution-test addition
  "define no-inline method f (x :: <integer>) x + 1 end; f(3)" => 4
end;

define execution-test constant
  "define constant $y = 1; define no-inline method f (x) x + $y end; f(3)" => 4
end;

define execution-test variable
  "define variable *y* = 1; define no-inline method f (x) x + *y* end; f(3)" => 4
end;

define execution-test size
  "define no-inline method f (x) size(x) end; f(#[1, 2, 3])" => 3
end;

define suite dfmc-execution-suite ()
  test execution-basic;
  test execution-addition;
  test execution-constant;
  test execution-variable;
  test execution-size;
end;
