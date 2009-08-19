module: dfmc-visualization
author: Hannes Mehnert
copyright: 2009, all rights reversed
synopsis: Dylan side of graphical visualization of DFM control flow graphs

define function report-progress (i1 :: <integer>, i2 :: <integer>,
                                 #key heading-label, item-label)
  //if (item-label[0] = 'D' & item-label[1] = 'F' & item-label[2] = 'M')
  //  format-out("%s %s\n", heading-label, item-label);
  //end;
end;

define function write-data (vis :: <dfmc-graph-visualization>, key :: <symbol>, e, #rest arguments)
  let form = e.form;
  let env = form.identifier;
  let cr = if (instance?(form, <&method>))
             let cr = form.model-creator; //sometimes model-creator is already a <compilation-record>
             unless (instance?(cr, <compilation-record>))
               cr := cr.form-compilation-record;
             end;
             cr;
           elseif (instance?(form, <top-level-form>))
             form.form-compilation-record;
           end;
  if (cr)
    let sr = cr.compilation-record-source-record;
    let loc = if (instance?(sr, <flat-file-source-record>))
                sr.source-record-location.locator-name
              else
                "unknown"
              end;
//    if (loc = "new-dispatch.dylan")

     if (member?(env, list("%remove-method-from-library", "automatic-finalization-function"),
              test: method(x, y) copy-sequence(x, end: min(x.size, y.size)) = y end))
      write-to-visualizer(vis, apply(list, key, env, arguments));
    end;
  else
    //uhm... shouldn't be here
  end;
end;

define method form (c :: type-union(<temporary>, <computation>))
  c.environment.form
end;

define method form (c :: <exit>)
  c.entry-state.form
end;

define method form (c :: type-union(<&accessor-method>, <&lambda>))
  if (instance?(c.environment, <lexical-environment>))
    c.environment.form
  else
    c
  end
end;

define method form (c :: <variable-defining-form>)
  c
end;

define method form (c :: <lambda-lexical-environment>)
  (c.outer & c.outer.form) | c.lambda
end;

define method form (c :: <local-lexical-environment>)
  c.outer.form
end;

define constant $lambda-string-table = make(<table>);
//define constant $string-lambda-table = make(<table>);

define constant $top-l-i = "top-level-initializer";

define method identifier (f) => (res :: <string>)
  let id = as(<string>, f.form-variable-name);
  if (copy-sequence(id, end: min(id.size, $top-l-i.size)) = $top-l-i)
    let name = element($lambda-string-table, f, default: #f);
    unless (name)
      $lambda-string-table[f] := concatenate(id, integer-to-string($lambda-string-table.size));
    end;
    $lambda-string-table[f]
  else
    id
  end;
end;

define method identifier (f :: <name-fragment>) => (res :: <string>)
  as(<string>, f.fragment-name)
end;

define method identifier (m :: <method-definition>) => (res :: <string>)
  let str = make(<string-stream>, direction: #"output");
  print-specializers(m.form-signature, str);
  concatenate(as(<string>, m.form-variable-name), " ", str.stream-contents)
end;

define method identifier (l :: type-union(<&accessor-method>, <&lambda>)) => (res :: <string>)
  if (instance?(l.model-creator, <compilation-record>))
//type-union(<top-level-init-form>, <compilation-record>)))
    l.debug-name.identifier
  elseif (instance?(l.model-creator, <top-level-init-form>))
    let id = l.debug-name.identifier;
    if (copy-sequence(id, end: min(id.size, $top-l-i.size)) = $top-l-i)
      let name = element($lambda-string-table, l, default: #f);
      unless (name)
        $lambda-string-table[l] := concatenate(id, integer-to-string($lambda-string-table.size));
      end;
      $lambda-string-table[l]
    else
      id
    end;
  else
    l.model-creator.identifier
  end;
end;

define method identifier (s :: <string>) => (res :: <string>)
  s
end;

define constant form-id = compose(identifier, form);

define method get-id (c :: <computation>) => (id :: <integer>)
  c.computation-id
end;

define method get-id (t /* :: <temporary> */) => (id :: <integer>)
  t.temporary-id
end;

define method get-id (i :: <integer>) => (res :: <integer>)
  i
end;

define function trace-computations (vis :: <dfmc-graph-visualization>, key :: <symbol>, id, comp-or-id, comp2, #key label)
  select (key by \==)
    #"add-temporary-user", #"remove-temporary-user" =>
      write-data(vis, key, comp-or-id, id.get-id, comp-or-id.get-id);
    #"add-temporary" =>
      begin
        let str = make(<string-stream>, direction: #"output");
        print-object(id, str);
        write-data(vis, key, if (comp-or-id == 0) id else comp-or-id end,
                   id.get-id, str.stream-contents, 0);
      end;
    #"temporary-generator" =>
      write-data(vis, key, comp-or-id, id.get-id, comp-or-id.get-id, comp2.get-id);
    #"remove-temporary" =>
      write-data(vis, key, if (comp-or-id == 0) id else comp-or-id end, id.get-id);        
    #"remove-edge", #"insert-edge" =>
      write-data(vis, key, id, id.get-id, comp-or-id.get-id, label);
    #"change-edge" =>
      write-data(vis, key, id, id.get-id, comp-or-id.get-id, comp2.get-id, label);
    #"new-computation" =>
      write-data(vis, key, comp-or-id, output-computation-sexp(comp-or-id));
    #"remove-computation" =>
      write-data(vis, key, id, id.get-id);
    #"change-entry-point" =>
      write-data(vis, key, id, id.get-id, comp-or-id);
    #"change-function" =>
      begin
        let str = make(<string-stream>, direction: #"output");
        print-object(comp-or-id, str);
        write-data(vis, key, id, id.get-id, str.stream-contents);
      end;
    #"set-loop-call-loop" =>
      write-data(vis, key, id, id.get-id, comp-or-id, #"no");
    otherwise => ;
  end;
end;

define function visualize (vis :: <dfmc-graph-visualization>, key :: <symbol>, object :: <object>)
  select (key by \==)
    #"dfm-header" =>
      write-data(vis, key, object.head, object.tail);
    #"beginning" =>
      write-data(vis, key, object.head, object.tail);
    #"relayouted" =>
      write-data(vis, key, object.head);
    #"highlight-queue" =>
      write-data(vis, key, object.head, object.tail);
    #"highlight" =>
      write-data(vis, key, object, object.get-id);
    #"source" =>
      write-data(vis, key, object.head, object.tail);
    otherwise => ;
  end;
end;

define function visualizing-compiler (vis :: <dfmc-graph-visualization>, project, #key parse?)
  let lib = project.project-current-compilation-context;
  block()
    dynamic-bind(*progress-library* = lib,
                 *dump-dfm-method* = curry(visualize, vis),
                 *computation-tracer* = curry(trace-computations, vis))
      with-progress-reporting(project, report-progress, visualization-callback: curry(visualize, vis))
        let settings =
          if (parse?)
            let subc = project-load-namespace(project, force-parse?: #t);
            for (s in subc using backward-iteration-protocol)
              parse-project-sources(s);
            end;
            let project2 = compilation-context-project(project-current-compilation-context(project));
            project-build-settings(project2);
          end;

        compile-library-from-definitions(lib, force?: #t, skip-link?: #t,
                                         compile-if-built?: #t, skip-heaping?: #t,
                                         build-settings: settings);
      end;
    end;
  exception (e :: <abort-compilation>)
  end
end;

