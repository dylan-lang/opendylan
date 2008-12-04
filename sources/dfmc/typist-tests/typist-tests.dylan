module: dfmc-typist-tests
synopsis: Tests which should succeed once the new typist is in place
author: Hannes Mehnert
copyright: 2008, all rights reversed

define function compile-library-until-optimized (lib)
  compile-library-from-definitions(lib, force?: #t, skip-link?: #t,
                                   compile-if-built?: #t, skip-heaping?: #t,
                                  // compile-until-type-inferred?: #t,
                                   abort-on-all-warnings?: #t);
end function;

define function static-type (lambda :: <&method>) => (res :: <type-estimate>)
  local method final-computation-type(c :: <&method>)
          let cache = make(<type-cache>);
          type-estimate-in-cache(c, cache);
          type-estimate-in-cache(final-computation(body(c)), cache)
        end;
  final-computation-type(lambda);
end;

define function compile-string (string :: <string>)
  debug-assert(instance?(string, <string>));
  // Compile a template & cut through the underbrush to the init form
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(string,
                               compiler: compile-library-until-optimized);
    let cr* = library-description-compilation-records(lib);
    // One for lib+mod defn & one for the source template.
    debug-assert(size(cr*) == 2, "Expected exactly 2 <compilation-record>s: %=", cr*);
    let tlif = last(compilation-record-top-level-forms(cr*[1]));
    debug-assert(instance?(tlif, <top-level-init-form>),
                 "Expected %= to be a <top-level-init-form>", tlif);
    form-init-method(tlif)
  end
end;

define method \= (te1 :: <type-estimate>, te2 :: <type-estimate>)
 => (equal? :: <boolean>)
  //type-estimate=?(te1, te2);
  format-out("comparing %= with %=\n", te1, te2);
  let (sub?, known?) = type-estimate-subtype?(te1, te2);
  if (sub?)
    let (sub2?, known2?) = type-estimate-subtype?(te2, te1);
    sub2?;
  else
    sub?;
  end;
end;


define test noop ()
  let mycode = "concatenate(1, 2);";
  dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                *demand-load-library-only?* = #f)
    let lib = compile-template(mycode, compiler: compile-library-until-optimized);
    //format-out("LIB %= %=\n", lib, concatenate(1, 2));
    //let warn = lib.library-description-project.project-warnings;
    //format-out("WARN %= %=\n", warn.size, warn);
  end;
//    check-condition("concatenating 1 and 2",
//                    <abort-compilation>, 
    //<wrong-type-in-assignment>?
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
                    <warning>, compile-template(mycode, compiler: compile-library-until-optimized));
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
                    <warning>, compile-template(mycode, compiler: compile-library-until-optimized));
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
  test reduce-literal-limited-list;
  test map-limited-list;

  //tests for occurence typing

  //tests which should succeed once literals are typed "better"
  test literal-limited-list;
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

