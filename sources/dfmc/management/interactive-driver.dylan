module: dfmc-management
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function install-interactive-layer-sources
    (layer :: <interactive-layer>, sr*)
  // see install-library-description-sources
  let cr* = map-as(<compilation-record-vector>,
		   method (sr :: <source-record>)
		     make(<interactive-compilation-record>,
			  library: #f,
			  source-record: sr)
		   end method,
		   sr*);
  let known = layer.interactive-layer-base.library-description-compilation-records.size;
  // Number the compilation records to be contiguous with the known set, so that 
  // the "name" of the compilation record doesn't change when there's a merge.
  for (cr in cr*, index from known by 1)
    cr.compilation-record-sequence-number := index;
  end;
  layer.compilation-context-records := cr*;
end function;

// TODO: Should this be elsewhere?
define method note-definitions-updated (layer :: <interactive-layer>) => ()
  // Interactive compilation doesn't invalidate databases.
end method;

/*
in: dfmc-debug;

define function itest-sr (text, #key module = "internal")
 make(access(projects-implementation,<string-template-source-record>),
      contents: as(<byte-vector>,text),
      module: as(<symbol>, module),
      name: "Test");
end function;

define function itest (text, #key module = "internal",
		                  library = "dylan",
		                  target = list("New target"))
  execute-source
    (lookup-interactive-context(target, lookup-library-description(library)),
     #"no-context",
     list(itest-sr(text, module: module)));
end function;

define function htest (text, #key module = "functional-extensions-internals",
		                  library = "functional-extensions",
		                  target = list("New target"))
  itest(text, module: module, library: library, target: target)
end function;
*/

define function ensure-layer-compiled (layer :: <interactive-layer>, flags, #key heap? = #t)
  ensure-library-models-computed(layer);
  // ensure-library-models-finished(layer);
  // ensure-library-models-checked(layer);
  ensure-library-dfm-computed(layer);
  // ensure-library-bindings-checked(description);
  ensure-library-type-estimated(layer);
  ensure-library-optimized(layer);
  heap? & ensure-layer-heaps-computed(layer, flags);
end function;
  
define function ensure-layer-heaps-computed (layer :: <interactive-layer>,
					     flags :: <sequence>)
  debug-out(#"internal", "Heaping:");
  timing-compilation-phase ("Heaping" of layer)

    with-back-end-initialization(current-back-end())

    for (cr in compilation-context-records(layer))
      unless (cr.compilation-record-model-heap)
	progress-line("Computing heap for %s", cr);
	with-dependent ($compilation of cr)
	  apply(compute-and-install-compilation-record-heap, cr, flags);
        end;
      end;
    end;

    end with-back-end-initialization;

  end;
  debug-out(#"internal", "Heaping Done.");
end function;

// Part of documented API
define function execute-source
  (ild :: <interactive-library-description>,
   runtime-context,
   sr* :: <sequence>,
   #rest flags, #key skip-link?, harp-output?, interpret?, trace?, #all-keys)
 => transaction-id;
  with-program-conditions
    with-interactive-layer (layer = ild in runtime-context)
      dynamic-bind (*progress-library* = #f /* , *colorize-dispatch* = #f */)
        install-interactive-layer-sources(layer, sr*);
        debug-assert(~layer.compiled-to-definitions?);
        compute-library-definitions(layer);
        debug-assert(~any?(compilation-record-model-heap,
			   layer.compilation-context-records));
        ensure-layer-compiled(layer, flags, heap?: ~interpret?);
        // ALL SET, NOW DOWNLOAD!
        let tid
	  = if (interpret?)
	      ensure-library-interpreted(layer, trace?: trace?, results?: #t); 
	    else 
	      skip-link? | apply(link-and-download, current-back-end(),
				 layer, runtime-context, flags);
	    end if;
        merge-interactive-layer(layer, tid);
        tid
      end dynamic-bind;
    end with-interactive-layer;
  end with-program-conditions;
end function;

define function source-complete?
  (ild :: <interactive-library-description>,
   runtime-context,
   sr* :: <sequence>,
   #rest flags, #key, #all-keys)
 => (well? :: <boolean>, warnings :: <sequence>)
  // with-program-conditions
    with-interactive-layer (layer = ild in runtime-context)
      dynamic-bind (*progress-library* = #f /* , *colorize-dispatch* = #f */)
        // I'm kind of assuming things will clean up after themselves if
        // we just abort this way, before merging the layer. I hope that's
        // true.
        block ()
          install-interactive-layer-sources(layer, sr*);
          debug-assert(~layer.compiled-to-definitions?);
          compute-library-definitions(layer);
          debug-assert(~any?(compilation-record-model-heap,
                             layer.compilation-context-records));
          values(#t, #());
        exception (c :: <invalid-end-of-input>)
          values(#f, #());
          // To get a different kind of warning, it must have parsed??!
        exception (c :: <reader-error>)
          values(#t, list(c));
        exception (c :: <program-condition>)
          values(#t, #());
        end;
      end dynamic-bind;
    end with-interactive-layer;
  // end with-program-conditions;
end function;

define method execute-definition-removal
  (ild :: <interactive-library-description>,
   runtime-context,
   definition* :: <sequence>,
   #rest flags, #key skip-link?, harp-output?, #all-keys)
 => transaction-id;
  with-program-conditions
    with-interactive-layer (layer = ild in runtime-context)
      dynamic-bind (*progress-library* = #f /* , *colorize-dispatch* = #f */)
        break("execute-definition-removal stub");
      end dynamic-bind;
    end with-interactive-layer;
  end with-program-conditions;
end /* function */;

define method macroexpand-source 
    (ld :: <library-description>, sr :: <source-record>,
       #key expansion-stream :: false-or(<stream>) = #f, 
            trace-stream :: false-or(<stream>) = #f)
 => (warnings :: <sequence>)
  let ild = lookup-interactive-context(#"dummy-macroexpansion-target", ld);
  block ()
    macroexpand-source(ild, sr, 
                       expansion-stream: expansion-stream,
                       trace-stream: trace-stream);
  cleanup
    close-library-description(ild);
  end;
end method;

define method macroexpand-source 
    (ild :: <interactive-library-description>, sr :: <source-record>,
       #key expansion-stream :: false-or(<stream>) = #f, 
            trace-stream :: false-or(<stream>) = #f)
 => (warnings :: <sequence>)
  let sr* = list(sr);
  // with-program-conditions
    with-interactive-layer (layer = ild in #f)
      dynamic-bind (*progress-library* = #f /* , *colorize-dispatch* = #f */)
        // I'm kind of assuming things will clean up after themselves if
        // we just abort this way, before merging the layer. I hope that's
        // true.
        block ()
          install-interactive-layer-sources(layer, sr*);
          debug-assert(~layer.compiled-to-definitions?);
          with-macroexpansion-output 
              (expansion-stream: expansion-stream, trace-stream: trace-stream)
            compute-library-definitions(layer);
          end;
          debug-assert(~any?(compilation-record-model-heap,
                             layer.compilation-context-records));
          #();
        exception (c :: <program-condition>)
          if (expansion-stream)
            format(expansion-stream, "\n<Failed to parse for macroexpansion>");
          end;
          list(c);
        end;
      end dynamic-bind;
    end with-interactive-layer;
  // end with-program-conditions;
end method;


///---*** NOTE: Is this the right place to add these?

define open generic interpret-top-level-form
    (form :: <top-level-form>, #key trace? = #f) => (transaction-id);

define open generic unregister-interpreter-transaction
    (transaction-id) => ();

define open generic ensure-library-interpreted
    (description :: <library-description>, #key trace? = #f, results? = #f)
 => ();
