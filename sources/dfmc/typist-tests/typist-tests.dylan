module: dfmc-typist-tests
synopsis: Tests which should succeed once the new typist is in place
author: Hannes Mehnert
copyright: 2008, all rights reversed

define function callback-handler (#rest args)
  format-out("%=\n", args);
end function callback-handler;

define constant $tests = make(<stretchy-vector>);

begin
  let if-nested
    = "define method if-nested (x, y, z)\n"
      "  if (x == 1)\n"
      "    if (y == 1)\n"
      "      if (z == 1)\n"
      "        \"all equal\";\n"
      "      else\n"
      "        \"x and y equal\";\n"
      "      end;\n"
      "    elseif (z == 2)\n"
      "      \"y + 1 is z\";\n"
      "    else\n"
      "      \"all different\";\n"
      "    end;\n"
      "  end;\n"
      "end;";
  add!($tests, pair(#"if-nested (<object>, <object>, <object>)", if-nested));
  
  let if-simple
    = "define method if-simple\n"
      " (a :: <integer>, b :: <integer>)\n"
      " => (res :: <integer>)\n"
      "  if (a == 23)\n"
      "    1 + a + b;\n"
      "  else\n"
      "    42 + 10;\n"
      "  end;\n"
      "end;";
  add!($tests, pair(#"if-simple (<integer>, <integer>)", if-simple));

  let common-sub =
    "define method common-subexpression (a, b)\n"
    "  values(a + b, (a + b) * b);\n"
    "end;";
  add!($tests, pair(#"common-subexpression (<object>, <object>)", common-sub));

  let common-sub2 =
    "define method common-subexpression2\n"
    " (a :: <integer>, b :: <integer>)\n"
    "  values(a + b, (a + b) * b);\n"
    "end;";
  add!($tests, pair(#"common-subexpression2 (<integer>, <integer>)", common-sub2));

  let whil-true =
    "define method while-true-loop ()\n"
    "  while(#t)\n"
    "    1 + 2;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"while-true-loop ()", whil-true));

  let lfor =
    "define method for-loop (x, y)\n"
    "  for (i from 0 below 20)\n"
    "    x := y + 1;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"for-loop (<object>, <object>)", lfor));

  let whill =
    "define method while-loop ()\n"
    "  let i = 0;\n"
    "  while(i < 42)\n"
    "    i := i + 1;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"while-loop ()", whill));

  let whilln =
    "define method while-loop-nested ()\n"
    "  let i = 0;\n"
    "  while(i < 42)\n"
    "    i := i + 1;\n"
    "    while (i < 20)\n"
    "      i := i * i;\n"
    "    end;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"while-loop-nested ()", whilln));

  let blte =
    "define method block-test ()\n"
    "  block(t)\n"
    "    t();\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"block-test ()", blte));

  let ble =
    "define method block-exception (x)\n"
    "  block()\n"
    "    x := x * x;\n"
    "  exception (c :: <condition>)\n"
    "    x := 0;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"block-exception (<object>)", ble));

  let blcl =
    "define method block-cleanup (x, y)\n"
    "  block(t)\n"
    "    if (x == 42)\n"
    "      t();\n"
    "    end;\n"
    "    x := 20;\n"
    "    y := 42 * x;\n"
    "  cleanup\n"
    "    x := y;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"block-cleanup (<object>, <object>)", blcl));

  let db =
    "define method dyn-bind (x, y)\n"
    "  let t = 42;\n"
    "  dynamic-bind(t = 0)\n"
    "    x := t * t;\n"
    "  end;\n"
    "  y := t + t;\n"
    "  values(x, y);\n"
    "end;";
  add!($tests, pair(#"dyn-bind (<object>, <object>)", db));

  let inl =
    "define method double\n (a :: <integer>)\n"
    "  2 * a;\n"
    "end;";
  add!($tests, pair(#"double (<integer>)", inl));

  let dead-code =
    "define method dead ()\n"
    "  if (#f)\n"
    "    23;\n"
    "  else\n"
    "    42;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"dead ()", dead-code));

  let cse =
    "define method cse\n (a :: <integer>)\n"
    "  values(42 + a, (42 + a) * 2);\n"
    "end;";
  add!($tests, pair(#"cse (<integer>)", cse));

  let tc =
    "define method tail-call\n (x :: <integer>)\n"
    "  if (x == 0)\n"
    "    1\n"
    "  else\n"
    "    tail-call(x - 1)\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"tail-call (<integer>)", tc));

  let mymap =
    "define method map-vector ()\n"
    "  local method mymap\n"
    "   (fun :: <function>, l :: <vector>)\n"
    "   => (res :: <vector>)\n"
    "    if (l.empty?)\n"
    "      #[]\n"
    "    else\n"
    "     let tail = copy-sequence(l, start: 1);\n"
    "     vector(fun(l.first),\n"
    "            mymap(fun, tail))\n"
    "    end\n"
    "  end;\n"
    "  mymap(method(x) x + 1 end, #[1, 2, 3]);\n"
    "end;";
  add!($tests, pair(#"map-vector ()", mymap));
end;

/*
begin
  let top-build = "c:\\vis-stage3\\";
  environment-variable("OPEN_DYLAN_USER_ROOT") := top-build;
  environment-variable("OPEN_DYLAN_USER_BUILD") := concatenate(top-build, "build");
  environment-variable("OPEN_DYLAN_USER_INSTALL") := top-build;
  environment-variable("OPEN_DYLAN_USER_REGISTRIES") := "c:\\opendylan-visualization\\registry";
  let vis = make(<dfmc-graph-visualization>, id: #"Dylan-Graph-Visualization");
  connect-to-server(vis);
  let project = lookup-named-project("dylan");
  visualizing-compiler(vis, project, parse?: #t);
end;
*/

begin
  let project = find-project("dylan");
  open-project-compiler-database(project,
                                 warning-callback: callback-handler,
                                 error-handler: callback-handler);
  with-library-context (dylan-library-compilation-context())
    without-dependency-tracking
      //with-open-file (file = "/home/visualization/log", if-exists: #"append", direction: #"output")
        let vis = make(<dfmc-graph-visualization>, id: #"Dylan-Graph-Visualization");
        connect-to-server(vis);
        for (test in $tests)
          write-to-visualizer(vis, list(#"source", as(<string>, test.head), test.tail));
        end;
        //format(file, "%s new connection %=\n", as-iso8601-string(current-date()), vis.system-info);
	//force-output(file);
        block()
          while (#t)
            let res = read-from-visualizer(vis); //expect: #"compile" "source"
            if (res[0] == #"compile")
              dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                            *demand-load-library-only?* = #f)
                //format(file, "compiling %s\n", res[1]);
                compile-template(res[1], compiler: curry(visualizing-compiler, vis));
              end;
            end;
          end;
        exception (e :: <condition>)
          format-out("received exception: %=\n", e);
        end;
      //end;
    end;
  end;
end;


define function list-all-package-names ()
  let res = #();
  local method collect-project
            (dir :: <pathname>, filename :: <string>, type :: <file-type>)
          if (type == #"file" & filename ~= "Open-Source-License.txt")
            if (last(filename) ~= '~')
              unless (any?(curry(\=, filename), res))
                res := pair(filename, res);
              end;
            end;
          end;
        end;
  let regs = find-registries($machine-name, $os-name);
  let reg-paths = map(registry-location, regs);
  for (reg-path in reg-paths)
    if (file-exists?(reg-path))
      do-directory(collect-project, reg-path);
    end;
  end;
  res;
end;
/*
begin
  let projects = list-all-package-names();
  *vis* := make(<dfmc-graph-visualization>, id: #"Dylan-Graph-Visualization");
  connect-to-server(*vis*);
  for (project in projects)
    write-to-visualizer(*vis*, list(#"project", project));
  end;
  block()
    let project = #f;
    while (#t)
      let res = read-from-visualizer(*vis*); //expect: #"compile" "source"
      if (res[0] == #"open-project")
        project := find-project(res[1]);
        open-project-compiler-database(project, 
                                       warning-callback: callback-handler,
                                       error-handler: callback-handler);
        //canonicalize-project-sources(project, force-parse?: #t);
        *current-index* := 0;
        *vis*.dfm-report-enabled? := #t;
        //send top-level-definitions
        //(#"source", method-name, compilation-record-id, source)
        *vis*.dfm-report-enabled? := #f;
      elseif (res[0] == #"compile")
        with-library-context (dylan-library-compilation-context()) //get cc from project
          without-dependency-tracking
            *current-index* := *current-index* + 1;
            dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                          *demand-load-library-only?* = #f)
              compile-template(res[1], compiler: compiler);
              //actually, find fragment and definition by id and call
              //top-level-convert-using-definition and/or use an interactive-layer
              //and call execute-source
              //be careful: probably need to hack top-level-convert-using-definition
              // to generate dfm even if we have a tight library
            end;
          end;
        end;
      end;
    end;
  exception (e :: <condition>)
    format-out("received exception: %=\n", e);
  end;
end
*/
