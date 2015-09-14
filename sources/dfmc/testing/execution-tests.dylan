Module:    dfmc-execution-testing
Synopsis:  Tests for the dfmc-execution.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $trace-execution = #f;
define constant $show-dfm = #f;

define macro execution-test-definer
  { define execution-test ?testname:name
      ?test-code:expression => ?result:expression
    end }
  => {
    define test "execution-" ## ?testname ()
      let (ld, tlf*) = compile-to-top-level-forms(?test-code);
      with-library-context (ld)
        if ($show-dfm)
          let output-stream = runner-output-stream(*runner*);
          let is = make(<indenting-stream>, inner-stream: output-stream);
          format(is, "\nCompiled:\n");
          with-indentation (is, 2)
            format(is, "%s", ?test-code);
          end with-indentation;
          new-line(is);
          format(is, "Into:\n");
          with-indentation (is, 2)
            for (tlf in tlf*)
              dynamic-bind (*print-method-bodies?* = #t)
                print-object(tlf, is);
                new-line(is);
                with-indentation (is, 2)
                  for (m in tlf.form-top-level-methods)
                    print-object(m, is);
                    new-line(is);
                  end for;
                end with-indentation;
              end
            end for;
          end with-indentation;
          format(is, "---\n");
        end if;
        let transaction-id = #f;
        for (tlf in tlf*)
          transaction-id := interpret-top-level-form(tlf,
                                                     trace?: $trace-execution);
        end;
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
  "define not-inline method f ()\n"
  "  1\n"
  "end;\n"
  "f()\n"
  => 1
end;

define execution-test addition
  "define not-inline method f (x :: <integer>)\n"
  "  x + 1\n"
  "end;\n"
  "f(3)"
  => 4
end;

define execution-test constant
  "define constant $y = 1;\n"
  "define not-inline method f (x)\n"
  "  x + $y\n"
  "end;\n"
  "f(3)"
  => 4
end;

define execution-test variable
  "define variable *y* = 1;\n"
  "define not-inline method f (x)\n"
  "  x + *y*\n"
  "end;\n"
  "*y* := 2;\n"
  "f(2)" => 4
end;

define execution-test size
  "define not-inline method f (x)\n"
  "  size(x)\n"
  "end;\n"
  "f(#[1, 2, 3])"
  => 3
end;

define execution-test float-addition
  "define not-inline method f (x)\n"
  "  x + 1.0\n"
  "end;\n"
  "f(2.0)"
  => 3.0
end;

define execution-test single-float-subtraction
  "define not-inline method f (x :: <single-float>)\n"
  "  x - 1.0s0\n"
  "end;\n"
  "f(2.0s0)"
  => 1.0s0
end;

define execution-test double-float-multiplication
  "define not-inline method f (x)\n"
  "  x * 2.0d0\n"
  "end;\n"
  "f(2.0d0)"
  => 4.0d0
end;

define execution-test string-size
  "define not-inline method f (x :: <byte-string>)\n"
  "  size(x)\n"
  "end;\n"
  "f(\"abc\")"
  => 3
end;

define execution-test string-index
  "define not-inline method f (x :: <byte-string>)\n"
  "  x[0]\n"
  "end;\n"
  "f(\"abc\")"
  => 'a'
end;

define execution-test string-concatenate
  "define not-inline method f (fun :: <function>, x :: <byte-string>, y :: <byte-string>)\n"
  "  apply(fun, vector(x, y))\n"
  "end;\n"
  "f(concatenate, \"abc\", \"def\")"
  => "abcdef"
end;

define execution-test for-loop
  "define not-inline method f (args)\n"
  "  let sum = 0;\n"
  "  for (x in args)\n"
  "    sum := sum + x;\n"
  "  end for;\n"
  "  sum;\n"
  "end;\n"
  "f(#[1, 2, 3])"
  => 6
end;

define execution-test block-exit-1
  "define not-inline method f (x)\n"
  "  block (return)\n"
  "    if (odd?(x))\n"
  "      return(x)\n"
  "    else\n"
  "      -1\n"
  "    end if\n"
  "  end block\n"
  "end;\n"
  "f(3)"
  => 3
end;

define execution-test block-exit-2
  "define not-inline method f (x)\n"
  "  block (return)\n"
  "    if (odd?(x))\n"
  "      return(x)\n"
  "    else\n"
  "      -1\n"
  "    end if\n"
  "  end block\n"
  "end;\n"
  "f(2)"
  => -1
end;

define execution-test multiple-functions
  "define not-inline function f (a)\n"
  "  a * 2\n"
  "end;\n"
  "define not-inline function g (b)\n"
  "  f(b) + b\n"
  "end;\n"
  "g(2)"
  => 6
end;

define suite dfmc-execution-suite ()
  test execution-basic;
  test execution-addition;
  test execution-constant;
  test execution-variable;
  test execution-size;
  test execution-float-addition;
  test execution-single-float-subtraction;
  test execution-double-float-multiplication;
  test execution-string-size;
  test execution-string-index;
  test execution-string-concatenate;
  test execution-for-loop;
  test execution-block-exit-1;
  test execution-block-exit-2;
  test execution-multiple-functions;
end;
