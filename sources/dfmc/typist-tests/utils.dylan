module: dfmc-typist-tests
synopsis: Tests which should succeed once the new typist is in place
author: Hannes Mehnert
copyright: 2008, all rights reversed

define thread variable *batch-compiling* :: <boolean> = #f;

define function compile-library-until-optimized (project)
  let lib = project.project-current-compilation-context;
  block()
    compile-library-from-definitions(lib, force?: #t, skip-link?: #t,
                                     compile-if-built?: #t, skip-heaping?: #t,
                                     compile-until-type-inferred?: #t)
  exception (e :: <abort-compilation>)
  end
end function;

define function report-progress (i1 :: <integer>, i2 :: <integer>,
                                 #key heading-label, item-label)
  //if (item-label[0] = 'D' & item-label[1] = 'F' & item-label[2] = 'M')
  //  format-out("%s %s\n", heading-label, item-label);
  //end;
end;

define thread variable *vis* :: false-or(<dfmc-graph-visualization>) = #f; 
define thread variable *current-index* :: <string> = "";

define function trace-computations (key :: <symbol>, id :: <integer>, comp-or-id, comp2 :: <integer>, #key label)
  select (key by \==)
    #"add-temporary-user", #"remove-temporary-user" =>
      write-to-visualizer(*vis*, list(key, *current-index*, id, comp-or-id));
    #"add-temporary" =>
      begin
        let str = make(<string-stream>, direction: #"output");
        print-object(comp-or-id, str);
        write-to-visualizer(*vis*, list(key, *current-index*, id, str.stream-contents, comp2));
      end;
    #"temporary-generator" =>
      write-to-visualizer(*vis*, list(key, *current-index*, id, comp-or-id, comp2));
    #"remove-temporary" =>
      write-to-visualizer(*vis*, list(key, *current-index*, id));        
    #"remove-edge", #"insert-edge" =>
      write-to-visualizer(*vis*, list(key, *current-index*, id, comp-or-id, label));
    #"change-edge" =>
      write-to-visualizer(*vis*, list(key, *current-index*, id, comp-or-id, comp2, label));
    #"new-computation" =>
      write-to-visualizer(*vis*, list(key, *current-index*, output-computation-sexp(comp-or-id)));
    #"remove-computation" =>
      write-to-visualizer(*vis*, list(key, *current-index*, id));
    #"change-type" =>
      begin
        let str = make(<string-stream>, direction: #"output");
        if (instance?(comp-or-id, <type-variable>))
          unless (comp-or-id.type-contents-callback)
            comp-or-id.type-contents-callback
              := rcurry(curry(trace-computations, #"change-type", id), 0);
          end;
          print-object(comp-or-id.type-variable-contents, str);
        else
          print-object(comp-or-id, str);
        end;
        write-to-visualizer(*vis*, list(key, *current-index*, id, str.stream-contents));
      end;
    #"change-entry-point" =>
      write-to-visualizer(*vis*, list(key, *current-index*, id, comp-or-id));
      //format-out("changing entry point of %d %=\n", id, comp-or-id);
    #"set-loop-call-loop" =>
      write-to-visualizer(*vis*, list(#"set-loop-call-loop", *current-index*, id, comp-or-id, #"no"));
    otherwise => ;
  end;
end;
define function visualize (key :: <symbol>, object :: <object>)
  select (key by \==)
    #"file-changed" => *vis*.report-enabled? := (object = "scratch-source") | *batch-compiling*;
    #"dfm-switch" => *vis*.dfm-report-enabled? := (object == 4) | *batch-compiling*;
    #"dfm-header" =>
        write-to-visualizer(*vis*, list(key, *current-index*, object));
    #"optimizing" =>
      begin
        *vis*.dfm-report-enabled? := (object == 4) | *batch-compiling*;
        write-to-visualizer(*vis*, list(#"relayouted", *current-index*));
      end;
    //#"finished" =>
    //  *vis*.dfm-report-enabled? := #f;
    #"beginning" =>
      write-to-visualizer(*vis*, list(key, *current-index*, object));
    #"relayouted" =>
      write-to-visualizer(*vis*, list(key, *current-index*));
    #"highlight-queue" =>
      write-to-visualizer(*vis*, list(key, *current-index*, object));
    #"highlight" =>
      if (instance?(object, <integer>))
        write-to-visualizer(*vis*, list(key, *current-index*, object));
      end;
    #"full-dfm" =>
      format-out("GOT DFM %=\n", object);
    #"source" =>
      *batch-compiling* & write-to-visualizer(*vis*, list(key, object.head, object.tail));
    #"choose-source" =>
      *batch-compiling* & write-to-visualizer(*vis*, list(key, object));
    otherwise => ;
  end;
end;

define function compiler (project, #rest keys, #key library, #all-keys)
  let lib = library | project.project-current-compilation-context;
  block()
    dynamic-bind(*progress-library* = lib)
      dynamic-bind(*dump-dfm-method* = visualize)
        dynamic-bind(*computation-tracer* = trace-computations)
          with-progress-reporting(project, report-progress, visualization-callback: visualize)
    //let subc = project-load-namespace(project, force-parse?: #t);
    //for (s in subc using backward-iteration-protocol)
    //  parse-project-sources(s);
    //end;
    //let project2 = compilation-context-project(project-current-compilation-context(project));
    //let settings = project-build-settings(project2);

            apply(compile-library-from-definitions, lib, force?: #t, skip-link?: #t,
                                             compile-if-built?: #t, skip-heaping?: #t,/* build-settings: settings, */ keys);
          end;
        end;
      end;
    end;
  exception (e :: <abort-compilation>)
  end
end;

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

define function collect-elements (table :: <table>) => (res :: <collection>)
  //we don't have flatten...
  let res = make(<stretchy-vector>);
  do(curry(do, curry(add!, res)), as(<list>, table));
  res;
end;


