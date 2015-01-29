module: dfmc-management
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Heaping and Linking

define function mark-library-exported-bindings
    (ld :: <project-library-description>) => ()
  enable-library-externally-visible-elements(ld);
  let library = language-definition(ld);
  without-dependency-tracking
    when (library-description-defined?(ld))
      let variable = namespace-model-variable(library);
      let binding = untracked-lookup-binding(variable);
      model-externally-visible?(binding) := #t;
    end;
    for (library-binding in library.namespace-local-bindings)
      let module = library-binding-value(library-binding);
      if (module & exported?(library-binding))
        let variable = namespace-model-variable(module);
        let binding = untracked-lookup-binding(variable);
        model-externally-visible?(binding) := #t;
        // Directly exported bindings
        for (name in exported-names(module))
          let binding = lookup-name(module, name);
          if (untracked-binding-definition(binding, default: #f))
            model-externally-visible?(binding) := #t;
          end;
        end;
        // Re-exported bindings.
        for (binding in exported-imports-table(module))
          if (untracked-binding-definition(binding, default: #f))
            model-externally-visible?(binding) := #t;
          end;
        end;
      end;
    end;
  end;
end function;


define function ensure-library-heaps-computed (ld :: <project-library-description>,
                                               flags :: <sequence>)
 => (data-size :: <integer>, code-size :: <integer>)
  block ()
    debug-out(#"internal", "Heaping and Linking:\n");
    apply(compute-install-link-library-heaps, ld, flags);
  cleanup
    debug-out(#"internal", "Heaping and Linking done\n");
  end block;
end function;

define method compute-install-link-library-heaps
    (description :: <project-library-description>,
     #rest flags, #key skip-elimination?, #all-keys)
 => (data-size :: <integer>, code-size :: <integer>)
  let zap-dead-data?
    = ~library-forms-dynamic?(description) & ~skip-elimination?;
  if (zap-dead-data?)
    apply(tightly-link-library-heaps, description, flags)
  else
    apply(loosely-link-library-heaps, description, flags)
  end;
end method;

define variable *combine-object-files?* = #f;

define method tightly-link-library-heaps
    (description :: <project-library-description>,
     #rest flags, #key skip-link?, skip-emit?, #all-keys)
 => (data-size :: <integer>, code-size :: <integer>)
 if (*combine-object-files?*)
   let name = concatenate("_",
                          as(<string>,
                             description.library-description-emit-name));
   let cr = make(<library-compilation-record>,
                 library: description, source-record: #f, name: name);
   cr.compilation-record-sequence-number := $maximum-integer;
   description.library-description-combined-record := cr;
   timing-compilation-phase ("Heaping" of description)
     progress-line("Heaping %s", cr);
     mark-library-exported-bindings(description);
     compute-and-install-compilation-record-heap(cr, skip-emit?: #t);
   end;
   maybe-collect-and-dump-call-sites-from(description, cr);
   unless (skip-emit?)
     timing-compilation-phase ("Emitting" of description)
       progress-line("Emitting %s", cr);
       with-back-end-initialization(current-back-end())
         apply(emit-compilation-record-heap, cr, flags);
       end with-back-end-initialization;
     end;
   end;
   unless (skip-link?)
    // As soon as we start linking, last build becomes invalid, so clear it.
    description.library-description-built? := #f;
    timing-compilation-phase ("Linking" of description)
      apply(emit-library-record, current-back-end(), cr, description, flags);
    end;
   end unless;
   values(compilation-record-data-size(cr), compilation-record-code-size(cr))
 ELSE
  // Fill in the cross-refs
  timing-compilation-phase ("Preheaping" of description)
    mark-library-exported-bindings(description);
    precompute-library-heaps(description);
  end;
  // Compute heaps
  timing-compilation-phase ("Heaping" of description)
    compute-library-reachable-heap(description);
  end;
  maybe-collect-and-dump-call-sites(description);

  let data-size :: <integer> = 0;
  let code-size :: <integer> = 0;

  with-back-end-initialization(current-back-end())

  for (cr in compilation-context-records(description))
    let name = cr.compilation-record-source-record.source-record-name;
    source-record-progress-text("Generating code for %s", name);
    unless (skip-emit?)
      timing-compilation-phase ("Emitting" of description, progress?: #f, accumulate?: #t)
        progress-line("  Emitting heap for %s.dylan", name);
        apply(emit-compilation-record-heap, cr, flags);
        data-size := data-size + compilation-record-data-size(cr);
        code-size := code-size + compilation-record-code-size(cr);
      end;
    end;
    unless (skip-link?)
      // As soon as we start linking, last build becomes invalid, so clear it.
      description.library-description-built? := #f;
      timing-compilation-phase ("Linking" of description, progress?: #f, accumulate?: #t)
        apply(emit-library-record, current-back-end(), cr, description, flags);
      end;
    end unless;
    source-record-progress-report();
  end;

  end with-back-end-initialization;

  values(data-size, code-size)
 END;
end method;

define method loosely-link-library-heaps
    (description :: <project-library-description>,
     #rest flags, #key start-at, skip-link?, #all-keys)
 => (data-size :: <integer>, code-size :: <integer>)

  let cr* = if (~start-at)
              compilation-context-records(description)
            else
              choose(method (cr) cr.compilation-record-name = start-at end,
                     compilation-context-records(description))
            end;
  let call-sites :: <object-table> = make(<table>);
  let data-size :: <integer> = 0;
  let code-size :: <integer> = 0;

  with-back-end-initialization(current-back-end())

  for (cr in cr*)
    when (start-at | cr.compilation-record-needs-linking?)
      let name = cr.compilation-record-source-record.source-record-name;
      source-record-progress-text("Generating code for %s", name);
      timing-compilation-phase ("Heaping" of description, progress?: #f, accumulate?: #t)
        progress-line("Computing heap for %s", cr);
        apply(compute-and-install-compilation-record-heap, cr, flags);
        data-size := data-size + compilation-record-data-size(cr);
        code-size := code-size + compilation-record-code-size(cr);
      end;
      maybe-collect-call-sites-using(cr, call-sites);
      unless (skip-link?)
        // As soon as we start linking, last build becomes invalid, so clear it.
        description.library-description-built? := #f;
        timing-compilation-phase ("Linking" of description, progress?: #f, accumulate?: #t)
          apply(emit-library-record, current-back-end(), cr, description, flags);
        end;
      end unless;
      source-record-progress-report();
    end when;
  end for;

  end with-back-end-initialization;

  maybe-dump-call-sites(description, call-sites);
  values(data-size, code-size)
end method;

//// Linking.

define function ensure-library-glue-linked (ld :: <library-description>,
                                            build-settings :: <sequence>)
  debug-out(#"internal", "Emitting Glue: %s.\n", ld);
  timing-compilation-phase ("Glue gen" of ld)
    apply(emit-glue, current-back-end(), ld, build-settings)
  end;
end function;

//// profile information

define method print-terse-source-location
    (s :: <stream>, src-location, src-location-context)
  format(s, "[]");
end method;

define method print-terse-source-location
    (s :: <stream>, f :: <compiler-range-source-location>, sf :: false-or(<compiler-range-source-location>))
  format(s, "[");
  unless (sf & as(<symbol>, source-record-name(f.source-location-source-record))
                 == as(<symbol>, source-record-name(sf.source-location-source-record)))
    format(s, "%s ", source-record-name(f.source-location-source-record));
  end unless;
  format(s, "(%d, %d) - (%d, %d)]",
         f.source-location-start-offset.source-offset-line,
         f.source-location-start-offset.source-offset-column,
         f.source-location-end-offset.source-offset-line,
         f.source-location-end-offset.source-offset-column);
end method;

define method maybe-collect-and-dump-call-sites
    (ld :: <library-description>) => ()
  let call-sites = make(<object-table>);
  for (cr in compilation-context-records(ld))
    maybe-collect-call-sites-using(cr, call-sites);
  end for;
  maybe-dump-call-sites(ld, call-sites);
end method;

define method maybe-collect-and-dump-call-sites-from
    (ld :: <library-description>, cr :: <compilation-record>) => ()
  let call-sites = make(<object-table>);
  maybe-collect-call-sites-using(cr, call-sites);
  maybe-dump-call-sites(ld, call-sites);
end method;

define function lambda-source-location (object :: <&lambda>) => (loc)
  let body-spec = body-spec(object);
  let body      = body(object);
  if (body-spec)
    body-spec.fragment-source-location
  elseif (body)
    computation-source-location(body)
  else
    #f
  end if;
end function;

define function find-a-source-location(o :: <&lambda>) => (res)
  if (model-has-definition?(o))
    o.model-source-location
  else
    lambda-source-location(o)
  end if;
end function;

define method maybe-collect-call-sites-using
    (cr :: <compilation-record>, call-sites :: <object-table>) => ()
  when (*profile-all-calls?*)
    let heap = cr.compilation-record-model-heap;
    let literals = heap.heap-defined-object-sequence;
    for (literal in literals)
      when (instance?(literal, <&profiling-call-site-cache-header-engine-node>))
        let call = profiling-call-site-cache-header-engine-node-call(literal);
        let lambda = lambda(environment(call));
        let method-call-sites
          = element(call-sites, lambda, default: #f)
              | (element(call-sites, lambda) := make(<stretchy-object-vector>));
        add!(method-call-sites, literal);
      end when;
    end for;
  end when;
end method;

define method maybe-dump-call-sites
    (ld :: <library-description>, call-sites :: <object-table>) => ()
  when (*profile-all-calls?*)
    with-profile-area-output (stream = ld, type: "calls")
      for (lambda-call-sites keyed-by lambda in call-sites)
        print-referenced-object(lambda, stream);
        format(stream, " ");
        let lambda-location = find-a-source-location(lambda);
        print-terse-source-location(stream, lambda-location, #f);
        format(stream, "\n");
        local method call-site-number
                  (call-site :: <&profiling-call-site-cache-header-engine-node>) => (res :: <integer>)
                let call = profiling-call-site-cache-header-engine-node-call(call-site);
                let tmp  = temporary(call);
                if (tmp)
                  frame-offset(tmp)
                else
                  0
                end if
              end method,
              method compare-calls (cs1 :: <&profiling-call-site-cache-header-engine-node>,
                                    cs2 :: <&profiling-call-site-cache-header-engine-node>)
               => (well? :: <boolean>)
                call-site-number(cs1) < call-site-number(cs2)
              end method;
        sort!(lambda-call-sites, test: compare-calls);
        for (call-site in lambda-call-sites)
          let call = profiling-call-site-cache-header-engine-node-call(call-site);
          format(stream, "  %= ", ^profiling-call-site-cache-header-engine-node-id(call-site));
          let parent = ^cache-header-engine-node-parent(call-site);
          format(stream, "(%s) ", ^debug-name(parent) | "");
          print-terse-source-location(stream, computation-source-location(call), lambda-location);
          format(stream, "\n");
        end for;
      end for;
    end with-profile-area-output;
  end when;
end method;


