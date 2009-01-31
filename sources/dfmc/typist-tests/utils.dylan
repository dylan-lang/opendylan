module: dfmc-typist-tests
synopsis: Tests which should succeed once the new typist is in place
author: Hannes Mehnert
copyright: 2008, all rights reversed

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
define thread variable *current-index* :: <integer> = 0;

define thread variable *trace-edges* :: <boolean> = #f;

define function trace-computations (key :: <symbol>, id :: <integer>, comp-or-id :: type-union(<computation>, <integer>),
                                    comp2 :: <integer>)
  if (member?(id, #(170, 176, 171, 175)))
    format-out("got me\n");
  end;
  if (*trace-edges*)
    select (key by \==)
      #"remove-edge", #"insert-edge" =>
        write-to-visualizer(*vis*, list(key, *current-index*, id, comp-or-id));
      #"change-edge" =>
        write-to-visualizer(*vis*, list(key, *current-index*, id, comp-or-id, comp2));
      #"new-computation" =>
        write-to-visualizer(*vis*, list(key, *current-index*, output-computation-sexp(comp-or-id)));
      #"remove-computation" =>
        write-to-visualizer(*vis*, list(key, *current-index*, id));
    end;
  end;
end;
define function visualize (key :: <symbol>, object :: <object>)
//format-out("VIS %= %=\n", context, object);
  select (key by \==)
    #"initial-dfm" =>
      begin
        let id :: <integer> = object.head;
        write-to-visualizer(*vis*, list(key, id, object.tail.head));
        *current-index* := id;
      end;
    #"optimizing" =>
      begin
        *current-index* := object;
        *trace-edges* := #t;
        write-to-visualizer(*vis*, list(#"relayouted", *current-index*));
      end;
    #"finished" =>
      begin
        //format-out("now we could wait for input and process requests");
        *trace-edges* := #f;
        *current-index* := 0;
      end;
    #"relayouted" =>
      write-to-visualizer(*vis*, list(key, *current-index*));
    #"highlight-queue" =>
      write-to-visualizer(*vis*, list(key, *current-index*, object));
    #"highlight" =>
      if (instance?(object, <integer>))
        write-to-visualizer(*vis*, list(key, *current-index*, object));
      end;
    otherwise => ;
  end;
end;

define function compiler (project)
  *vis* := make(<dfmc-graph-visualization>, id: project.project-library-name);
  connect-to-server(*vis*);
  let lib = project.project-current-compilation-context;
  block()
    dynamic-bind(*progress-library* = lib)
      dynamic-bind(*dump-dfm-method* = visualize)
        dynamic-bind(*computation-tracer* = trace-computations)
          with-progress-reporting(project, report-progress, visualization-callback: visualize)
            compile-library-from-definitions(lib, force?: #t, skip-link?: #t,
                                             compile-if-built?: #t, skip-heaping?: #t);
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


