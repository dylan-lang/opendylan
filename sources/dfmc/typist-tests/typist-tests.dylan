module: dfmc-typist-tests
synopsis: Tests which should succeed once the new typist is in place
author: Hannes Mehnert
copyright: 2008, all rights reversed

define test noop ()
  let mycode = "concatenate(1, 2);";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    check-equal("one condition was reported", 1, size(conditions));
    let condition = conditions[0];
    check-instance?("it is a <argument-type-mismatch-in-call> object",
                    <argument-type-mismatch-in-call>, condition);
  end;
end;

define test visualization-demo ()
  let mycode =
   "define method if-test ()"
   "  let a :: <integer> = 23;"
   "  let b :: <integer> = 42;"
   "  if (instance?(a, <string>))"
   "    a := a * b;"
   "  else"
   "    a := a + b;"
   "  end;"
   "  a;"
   "end;";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    check-equal("no test to run", 0, 0);
  end;
end;

define test visualization-test ()
  let mycode = "define method if-nested (x, y, z)"
               "  if (x == 1)"
               "    if (y == 1)"
               "      if (z == 1)"
               "        \"all equal\";"
               "      else"
               "        \"x and y equal\";"
               "      end;"
               "    elseif (z == 2)"
               "      \"y + 1 is z\";"
               "    else"
               "      \"all different\";"
               "    end;"
               "  end;"
               "end;"
               "define method if-simple (a :: <integer>, b :: <integer>) => (res :: <integer>)"
               "  if (a == 23)"
               "    1 + a + b;"
               "  else"
               "    42 + 10;"
               "  end;"
               "end;"
               "define method common-subexpression (a, b)"
               "  values(a + b, (a + b) * b);"
               "end;"
               "define method for-loop (x, y, z)"
               "  for (i from 0 below 20)"
               "    x := y + 1;"
               "  end;"
               "end;"
//               "define method for-loop-multiple-variables ()"
//               "  let sum = 0;"
//               "  for (i from 0, j from 5, k from 20 above 0 by -1, l from 7 below 10)"
//               "    sum := i + j * k + l;"
//               "  end;"
//               "end;"
               "define method while-true-loop (x, y, z)"
               "  while(#t)"
               "    1 + 2;"
               "  end;"
               "end;"
               "define method while-loop (x, y, z)"
               "  let i = 0;"
               "  while(i < 42)"
               "    i := i + 1;"
               "  end;"
               "end;"
               "define method while-loop-nested (x, y, z)"
               "  let i = 0;"
               "  while(i < 42)"
               "    i := i + 1;"
               "    while (i < 20)"
               "      i := i * i;"
               "    end;"
               "  end;"
               "end;";
/*               "define method block-cleanup (x, y, z)"
               "  block(t)"
               "    if (x == 42)"
               "      t();"
               "    end;"
               "    x := 20;"
               "    y := 42 * x;"
               "  cleanup"
               "    x := y;"
               "  end;"
               "end;"
               "define method dyn-bind (x, y, z)"
               "  let t = 42;"
               "  dynamic-bind(t = 0)"
               "    x := t * t;"
               "  end;"
               "  y := t + t;"
               "  values(x, y);"
               "end;"; */
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    check-equal("no test to run", 0, 0);
  end;
end;

define test polymorphic-type-test0 ()
  let mycode = "define function my-+ (All(A)(x :: A, y :: A) => (res :: A))"
               "  x + y;"
               "end;";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    format-out("conditions: %=\n", conditions);
    check-equal("not-used condition was reported", 1, size(conditions));
  end;
end;

define test limited-function-type-test ()
  let mycode = "define function my-function (x :: <integer> => <string>, y :: <integer>) => (res :: <string>)"
               "  x(y);"
               "end;"
               "my-function(method(x :: <integer>) => (y :: <string>) \"foo\" end, 23);";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    format-out("conditions: %=\n", conditions);
    check-equal("no condition was reported", 0, size(conditions));
  end;
end;

define test limited-function-type-test2 ()
  let mycode = "define function my-function (x :: <integer> => <string>, y :: <integer>) => (res :: <string>)"
               "  x(y);"
               "end;"
               "my-function(method(x) \"foo\" end, 23);"; //here, <object> >= <integer>
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    format-out("conditions: %=\n", conditions);
    check-equal("no condition was reported", 0, size(conditions));
  end;
end;

define test limited-function-type-test3 ()
  let mycode = "define function my-function (x :: <integer> => <string>, y :: <integer>) => (res :: <string>)"
               "  x(y);"
               "end;"
               "my-function(method(x :: <string>) \"foo\" end, 23);"; // <string> ! >= <integer>
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    format-out("conditions: %=\n", conditions);
    check-equal("one condition was reported (wrong function type)", 1, size(conditions));
  end;
end;

define test polymorphic-type-test0a ()
  let mycode = "define function my-+ (All(A)(x :: A, y :: A) => (res :: A))"
               "  x + y;"
               "end;"
               "let my-increment = curry(my-+, 1);"
               "my-increment(\"foo\");";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    format-out("conditions: %=\n", conditions);
    check-equal("wrong type (<string> != <integer>) condition was reported", 1, size(conditions));
  end;
end;

define test polymorphic-type-test ()
  let mycode = "define function mymap (All(A, B)(fun :: A => B, c :: limited(<collection>, of: A)) => (res :: limited(<collection>, of: B)))"
               "  map(fun, c);"
               "end;"
               "define function my-+ (a :: <integer>, b :: <integer>) => (res :: <integer>)"
               "  a + b;"
               "end;"
               "begin"
               "  let incs = mymap(curry(my-+, 1), #(1, 2, 3));"
               "  if ((incs[0] ~= 2) | (incs[1] ~= 3) | (incs[2] ~= 4))"
               "    signal(make(<error>));"
               "  end;"
               "end;";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    check-equal("no conditions were reported", 0, size(conditions));
  end;
end;


define test reduce-literal-limited-list ()
  let mycode = "define function my-+ (a :: <integer>, b :: <integer>) => (res :: <integer>)"
               "  a + b;"
               "end;"
               "define function reduceme ()"
               "  let mylist :: limited(<list>, of: <symbol>) = #(#\"foo\", #\"bar\");"
               "  reduce1(my-+, mylist);"
               "end";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    check-equal("two conditions were reported (not-used and type-mismatch)", 2, size(conditions));
    let type-cons = choose(rcurry(instance?, <argument-type-mismatch-in-call>), conditions);
    check-equal("one condition is an argument-type-mismatch", 1, size(type-cons));
  end;
end;

define test map-limited-list ()
  let mycode = "define function my-+ (a :: <integer>, b :: <integer>) => (res :: <integer>)"
               "  a + b;"
               "end;"
               "define function map-limited-list (l :: limited(<collection>, of: <string>))"
               "  map(method(x) my-+(x, x) end, l);"
               "end;";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = as(<list>, lib.library-conditions-table);
    check-equal("two conditions were reported (not-used and type-mismatch)", 2, size(conditions));
    let type-cons = choose(rcurry(instance?, <argument-type-mismatch-in-call>), conditions);
    check-equal("one condition is an argument-type-mismatch", 1, size(type-cons));
  end;
end;

define test occurence-argument-wrong-typed ()
  let mycode = "define function my-+ (a :: <integer>, b :: <integer>) => (res :: <integer>)"
               "  a + b;"
               "end;"
               "define function occurence-argument-wrong-typed (x)"
               "  if (instance?(x, <symbol>))"
               "    my-+(x, x);"
               "  else"
               "    x;"
               "  end;"
               "end;";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    check-equal("three conditions were reported (not-used and one for each arg of my-+)", 3, size(conditions));
    let type-cons = choose(rcurry(instance?, <argument-type-mismatch-in-call>), conditions);
    check-equal("two conditions are argument-type-mismatch", 2, size(type-cons));
  end;
end;

define test occurence-argument-wrong-typed2 ()
  let mycode = "define function my-+ (a :: <integer>, b :: <integer>) => (res :: <integer>)"
               "  a + b;"
               "end;"
               "define function occurence-argument-wrong-typed2 (x)"
               "  if (instance?(x, <integer>))"
               "    x;"
               "  else"
               "    my-+(x, x);"
               "  end;"
               "end;";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    check-equal("three conditions were reported (not-used and one for each arg of my-+)", 3, size(conditions));
    let type-cons = choose(rcurry(instance?, <argument-type-mismatch-in-call>), conditions);
    check-equal("two conditions are argument-type-mismatch", 2, size(type-cons));
  end;
end;

define test literal-limited-list ()
  let lambda = 
    compile-string("define function literal-limited-list ()"
                   "  #(#\"bar\", #\"foo\", #\"barf\");"
                   "end; literal-limited-list();");
  let te = make(<type-estimate-values>,
                fixed: vector(make(<type-estimate-class>,
                                   class: dylan-value(#"<list>"))),
                rest: #f);
  let type = static-type(lambda);
  check-equal("type estimate for literal-list is a <list>", te, type);
  let te2 = make(<type-estimate-values>,
                 fixed: vector(make(<type-estimate-limited-collection>,
                                    class: dylan-value(#"<list>"),
                                    concrete-class: dylan-value(#"<list>"),
                                    of: make(<type-variable>,
                                             contents: make(<type-estimate-class>, class: dylan-value(#"<symbol>"))))),
                 rest: #f);
  check-equal("type estimate for literal-list is a limited list of symbols",
              te2, type);


  let te3 = make(<type-estimate-values>,
                 fixed: vector(make(<type-estimate-limited-collection>,
                                    class: dylan-value(#"<list>"),
                                    concrete-class: dylan-value(#"<list>"),
                                    size: 3)),
		 rest: #f);
  check-equal("type estimate for literal-list is a limited list of size 3",
              te3, type);

  let te4 = make(<type-estimate-values>,
                 fixed: vector(make(<type-estimate-limited-collection>,
                                    class: dylan-value(#"<list>"),
                                    concrete-class: dylan-value(#"<list>"),
                                    of: make(<type-variable>,
                                             contents: make(<type-estimate-class>, class: dylan-value(#"<symbol>"))),
                                    size: 3)),
                 rest: #f);
  check-equal("type estimate for literal-list is a limited list of symbol and size 3",
              te4, type);
end;



define suite typist-suite ()
  //tests for the test environment
//  test noop;
 // test visualization-demo;
  test visualization-test;

  //tests for limited function types
//  test limited-function-type-test;
//  test limited-function-type-test2;
//  test limited-function-type-test3;
/*
  //tests for type variable syntax
  test polymorphic-type-test0;
  test polymorphic-type-test0a;
  test polymorphic-type-test;

  //tests which should succeed with polymorphic types
  test reduce-literal-limited-list;
  test map-limited-list;

  //tests for occurence typing
  //the first works because instance is specially treated in optimization/assignment.dylan
  test occurence-argument-wrong-typed;
  test occurence-argument-wrong-typed2;

  //tests which should succeed once literals are typed "better"
  test literal-limited-list;
*/
  //how to check the amount of bound checks?
  //define function limited-vector-bounds-check ()
  //  //here, bounds checks are generated
  //  let foo = make(limited(<vector>, of: <single-float>, size: 3));
  //  foo[1] := foo[0];
  //end;
end;

define function callback-handler (#rest args)
  format-out("%=\n", args);
end function callback-handler;

begin
  let project = find-project("dylan");
  open-project-compiler-database(project, 
                                 warning-callback: callback-handler,
                                 error-handler: callback-handler);
  with-library-context (dylan-library-compilation-context())
    without-dependency-tracking
      run-test-application(typist-suite)
    end;
  end;
end

