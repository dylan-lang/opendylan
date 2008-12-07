module: dfmc-typist-tests
synopsis: Tests which should succeed once the new typist is in place
author: Hannes Mehnert
copyright: 2008, all rights reversed

define test noop ()
  let mycode = "concatenate(1, 2);";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compiler);
    let conditions = as(<list>, lib.library-conditions-table);
    check-equal("one condition was reported", 1, size(conditions));
    let condition = conditions[0].pop;
    check-instance?("it is a <argument-type-mismatch-in-call> object",
                    <argument-type-mismatch-in-call>, condition);
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
    check-condition("reducing literal list of symbols with a function which signature contains only <integer>",
                    <warning>, compile-template(mycode, compiler: compiler));
                    //<wrong-type-in-assignment>?
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
    check-condition("map list of strings with a function which signature contains only <integer>",
                    <warning>, compile-template(mycode, compiler: compiler));
                    //<wrong-type-in-assignment>?
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

  //tests which should succeed with polymorphic types
  //test reduce-literal-limited-list;
  //test map-limited-list;

  //tests for occurence typing

  //tests which should succeed once literals are typed "better"
  //test literal-limited-list;
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

