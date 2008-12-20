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

define test polymorphic-type-test0 ()
  let mycode = "define function my-+ (All(A)(a :: A, b :: A) => res :: A)"
               "  a + b;"
               "end;";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = collect-elements(lib.library-conditions-table);
    check-equal("not-used conditions was reported", 1, size(conditions));
  end;
end;
define test polymorphic-type-test ()
  let mycode = "define function mymap (All(A, B)(fun :: A => B, c :: limited(<collection>, of: A)) => res :: limited(<collection>, of: B))"
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
  test noop;

  //test for type variable syntax
  test polymorphic-type-test0;
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

