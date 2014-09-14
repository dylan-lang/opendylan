module: dispatch-profiler
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define primary class <profile-weight> (<object>)
  slot profile-hits :: <abstract-integer> = 0, init-keyword: hits:;
  slot profile-weighted-hits :: <abstract-integer> = 0, init-keyword: weighted-hits:;
end class;

define method profile-hit? (profile :: <profile-weight>) => (well? :: <boolean>)
  profile-hits(profile) > 0
end method;

define method profile-number-members (weight :: <profile-weight>) => (res :: <integer>)
  0
end method;

define method add-method-weight! (weight :: <profile-weight>, method-weight :: <dws-method-weight>)
  incf(profile-hits(weight), dmw-hits(method-weight));
  incf(profile-weighted-hits(weight), dmw-weighted-hits(method-weight));
end method;

define primary class <profile-weight-and-size> (<profile-weight>)
  slot profile-size :: <integer> = 0, init-keyword: size:;
end class;

define primary class <profile-weight-and-size-and-cache> (<profile-weight-and-size>)
  slot profile-cache-attempts :: <abstract-integer> = 0, init-keyword: cache-attempts:;
  slot profile-cache-hits :: <abstract-integer> = 0, init-keyword: cache-hits:;
end class;

define method add-cache-weight! (weight :: <profile-weight-and-size-and-cache>, dws :: <dispatch-walker-state>)
  incf(profile-cache-attempts(weight), dws-cache-attempts(dws));
  incf(profile-cache-hits(weight), dws-cache-hits(dws));
end method;

define primary class <profile-weight-and-size-and-cache-and-poly> (<profile-weight-and-size-and-cache>)
  slot profile-poly :: <integer> = 0;
end class;

define class <application-profile-results> (<profile-weight-and-size-and-cache-and-poly>)
  constant slot profile-library-results :: <object-table> = make(<table>);
  constant slot profile-generic-results :: <object-table> = make(<table>);
  constant slot profile-walked-generics :: <object-table> = make(<table>);
  constant slot profile-walked-caches :: <object-table>   = make(<table>);
  constant slot profile-shared-generic-caches? :: <boolean>,
    required-init-keyword: shared-generic-caches?:;
end class;

define method number-of-generics (results :: <application-profile-results>) => (res :: <integer>)
  size(profile-generic-results(results))
end method;

define method profile-number-static-calls (weight :: <application-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-static-calls, profile-library-results(weight)))
end method;

define method profile-number-dynamic-calls (weight :: <application-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-dynamic-calls, profile-library-results(weight)))
end method;

define method profile-number-members (weight :: <application-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-members, profile-library-results(weight)))
end method;

define method profile-number-call-sites (weight :: <application-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-call-sites, profile-library-results(weight)))
end method;

define method map-profile-call-sites (f :: <function>, weight :: <application-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(curry(map-profile-call-sites, f), profile-library-results(weight)))
end method;

define method make-dispatch-statistics (shared-generic-caches?)
  make(<application-profile-results>,
       shared-generic-caches?: shared-generic-caches?)
end method;

define class <library-profile-results> (<profile-weight-and-size-and-cache-and-poly>)
  constant slot profile-generic-results :: <object-table> = make(<table>);
  constant slot profile-number-static-calls :: <integer>,
    required-init-keyword: number-static-calls:;
  constant slot profile-number-dynamic-calls :: <integer>,
    required-init-keyword: number-dynamic-calls:;
end class;

define method number-of-generics (results :: <library-profile-results>) => (res :: <integer>)
  size(profile-generic-results(results))
end method;

define method profile-number-members (weight :: <library-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-members, profile-generic-results(weight)))
end method;

define method profile-number-call-sites (weight :: <library-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-call-sites, profile-generic-results(weight)))
end method;

define method map-profile-call-sites (f :: <function>, weight :: <library-profile-results>) => (res :: <integer>)
  reduce(\+, 0, map(curry(map-profile-call-sites, f), profile-generic-results(weight)))
end method;

define class <generic-profile-result> (<profile-weight-and-size-and-cache-and-poly>)
  constant slot generic-call-site-profile-results :: <stretchy-object-vector> = make(<stretchy-vector>);
end class;

define method profile-number-members (weight :: <generic-profile-result>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-members, generic-call-site-profile-results(weight)))
end method;

define method profile-number-call-sites (weight :: <generic-profile-result>) => (res :: <integer>)
  reduce(\+, 0, map(profile-number-call-sites, generic-call-site-profile-results(weight)))
end method;

define method map-profile-call-sites (f :: <function>, weight :: <generic-profile-result>) => (res :: <integer>)
  reduce(\+, 0, map(curry(map-profile-call-sites, f), generic-call-site-profile-results(weight)))
end method;

define class <call-site-profile-result> (<profile-weight-and-size-and-cache>)
  constant slot call-site-id :: <integer>, init-keyword: id:;
  constant slot call-site-library :: false-or(<library>), init-keyword: library:;
  constant slot call-site-types :: <simple-object-vector>, init-keyword: types:;
  constant slot call-site-method-profile-results :: <object-table> = make(<table>);
end class;

define method call-site-library-name (profile :: <call-site-profile-result>) => (res)
  let lib = call-site-library(profile);
  if (lib)
    namespace-name(lib)
  else
    "shared"
  end if
end method;

define method profile-number-members (weight :: <call-site-profile-result>) => (res :: <integer>)
  size(call-site-method-profile-results(weight))
end method;

define method profile-number-call-sites (weight :: <call-site-profile-result>) => (res :: <integer>)
  if (profile-hits(weight) > 0) 1 else 0 end;
end method;

define method map-profile-call-sites (f :: <function>, weight :: <call-site-profile-result>) => (res :: <integer>)
  f(weight)
end method;

define method clear-dispatch-statistics! (profile :: <application-profile-results>)
  remove-all-keys!(profile-generic-results(profile));
end method;

define method record-profile-result (app-result :: <application-profile-results>, dws :: <dispatch-walker-state>)
  let all-shared? = #f;
  let library
    = dws-library(dws);
  let library-result
    = element(profile-library-results(app-result), library, default: #f)
        | (element(profile-library-results(app-result), dws-library(dws))
             := make(<library-profile-results>,
                     number-static-calls:  if (library) library-number-static-dispatches(library)  else 0 end,
                     number-dynamic-calls: if (library) library-number-dynamic-dispatches(library) else 0 end));
  let generic-result
    = element(profile-generic-results(library-result), dws-generic(dws), default: #f)
        | (element(profile-generic-results(library-result), dws-generic(dws)) := make(<generic-profile-result>));
  let call-site-result
    = make(<call-site-profile-result>,
           library: dws-library(dws),
           types:   copy-sequence(dws-partial-types(dws)),
           size:    dws-size(dws),
           id:      dws-id(dws));
  profile-walked-generics(app-result)[dws-generic(dws)] := #t;
  let method-results = call-site-method-profile-results(call-site-result);
  add!(generic-call-site-profile-results(generic-result), call-site-result);
  let hit? = #f;
  let generic-result
    = element(profile-generic-results(library-result), dws-generic(dws), default: #f)
        | (element(profile-generic-results(library-result), dws-generic(dws)) := make(<generic-profile-result>));
  let call-site-result
    = make(<call-site-profile-result>,
           library: dws-library(dws),
           types:   copy-sequence(dws-partial-types(dws)),
           size:    dws-size(dws),
           id:      dws-id(dws));
  profile-walked-generics(app-result)[dws-generic(dws)] := #t;
  let method-results = call-site-method-profile-results(call-site-result);
  add!(generic-call-site-profile-results(generic-result), call-site-result);
  let hit? = #f;
  profile-poly(app-result)     := profile-poly(app-result)     + 1;
  profile-poly(library-result) := profile-poly(library-result) + 1;
  profile-poly(generic-result) := profile-poly(generic-result) + 1;
  for (method-weight keyed-by function in dws-method-weights(dws))
    method-results[function]
      := make(<profile-weight>,
              hits: dmw-hits(method-weight), weighted-hits: dmw-weighted-hits(method-weight));
    hit? := hit? | dmw-hits(method-weight) > 0;
    add-method-weight!(call-site-result, method-weight);
    add-method-weight!(generic-result, method-weight);
    add-method-weight!(library-result, method-weight);
    add-method-weight!(app-result, method-weight);
  end for;
  when (hit? | ~all-shared?)
    add-cache-weight!(call-site-result, dws);
    add-cache-weight!(generic-result,   dws);
    add-cache-weight!(library-result,   dws);
    add-cache-weight!(app-result,       dws);
    when (dws-library(dws) == #f & profile-shared-generic-caches?(app-result))
      profile-size(call-site-result) := profile-size(call-site-result) - cache-info-size(dws-generic(dws));
    end when;
    profile-size(app-result)     := profile-size(app-result)     + profile-size(call-site-result);
    profile-size(library-result) := profile-size(library-result) + profile-size(call-site-result);
    profile-size(generic-result) := profile-size(generic-result) + profile-size(call-site-result);
  end when;
end method;

define macro with-dispatch-profiling-disabled
  { with-dispatch-profiling-disabled ?:body end }
    => { let enabled? = *dispatch-profiling-enabled?*;
         block ()
           *dispatch-profiling-enabled?* := #f;
           ?body
         cleanup
           *dispatch-profiling-enabled?* := enabled?;
         end block }
end macro;


define method decache-generic (g :: <generic-function>) => ()
  // format-out("DECACHING %=\n", debug-name(g));
  g.discriminator := $absent-engine-node;
  let cache = %gf-cache(g);
  if (instance?(cache, <gf-cache-info>))
    let cache :: <gf-cache-info> = cache;
    for (user in gf-cache-info-users(cache))
      if (user)
        // let parent = cache-header-engine-node-parent(user);
        // let profiling-parent?
        //   = instance?(parent, <profiling-call-site-cache-header-engine-node>);
        // when (profiling-parent?)
        //   format-out("  %= %=\n",
        //              profiling-call-site-cache-header-engine-node-id(parent),
        //              namespace-name(profiling-call-site-cache-header-engine-node-library(parent)));
        // end when;
        let user :: <cache-header-engine-node> = user;
        cache-header-engine-node-next(user) := $absent-engine-node;
      end
    end for
  end if
end method;

define method decache-all-generics (library :: <library>)
  with-dispatch-profiling-disabled
    dispatch-walk-all-generics(library, decache-generic, make(<table>))
  end with-dispatch-profiling-disabled;
end method;


define method clear-dispatch-profiling (library :: <library>)
  with-dispatch-profiling-disabled
    dispatch-walk-all-engine-nodes(library, clear-dispatch-profiling-counters, make(<table>), make(<table>))
  end with-dispatch-profiling-disabled;
end method;


define method collect-dispatch-statistics (library :: <library>, profile :: <application-profile-results>)
  with-dispatch-profiling-disabled
    // dispatch-walk-all-generic-trees
    //   (library, curry(record-profile-result, profile), profile-walked-generics(profile), profile-walked-caches(profile));
    // remove-all-keys!(profile-walked-generics(profile));
    dispatch-walk-all-call-sites
      (library, curry(record-profile-result, profile), profile-walked-generics(profile), profile-walked-caches(profile));
    // dispatch-walk-library(library, curry(record-profile-result, profile));
  end with-dispatch-profiling-disabled;
end method;

define method print-specializer (stream :: <stream>, x :: <type>, readable?)
  format(stream, "%=", x);
end method;

define method print-specializer(stream :: <stream>, x :: <class>, readable?)
  format(stream, "%s", debug-name(x));
  when (readable?)
    let module-name  = namespace-name(class-module(x));
    let library-name = namespace-name(home-library(class-module(x)));
    if (module-name = library-name)
      if (module-name = "dylan")
        format(stream, ":::");
      else
        format(stream, "::%s", module-name);
      end if
    else
      format(stream, ":%s:%s", module-name, library-name);
    end if
  end when;
end method;

define method print-specializer(stream :: <stream>, x :: <singleton>, readable?)
  format(stream, "singleton(%=)", singleton-object(x));
end method;

define method print-specializer(stream :: <stream>, x :: <subclass>, readable?)
  format(stream, "subclass(");
  print-specializer(stream, subclass-class(x), readable?);
  format(stream, ")");
end method;

define method print-flattened-union-component (stream :: <stream>, x :: <type>, readable?)
  print-specializer(stream, x, readable?)
end method;

define method print-flattened-union-component
    (stream :: <stream>, x :: <union>, readable?)
  print-flattened-union-component(stream, union-type1(x), readable?);
  format(stream, ", ");
  print-flattened-union-component(stream, union-type2(x), readable?);
end method;

define method print-specializer(stream :: <stream>, x :: <union>, readable?)
  format(stream, "type-union(");
  print-flattened-union-component(stream, x, readable?);
  format(stream, ")");
end method;

define method walk-classes (f :: <function>)
  let seen? :: <object-table> = make(<table>);
  iterate search (c :: <class> = <object>)
    unless (element(seen?, c, default: #f))
      seen?[c] := #t;
      f(c);
      for (sc :: <class> in direct-subclasses(c))
        search(sc);
      end for;
    end unless;
  end iterate;
end method;

define method number-of-classes () => (res :: <integer>)
  let n :: <integer> = 0;
  walk-classes(method (c) n := n + 1 end);
  n
end method;

define method number-subclass-dependent-generics () => (res :: <integer>)
  let n :: <integer> = 0;
  local method number-dependent-generics (c :: <class>) => (res :: <integer>)
          let ic = class-implementation-class(c);
          size(iclass-subclass-dependent-generics(ic))
        end method;
  walk-classes(method (c :: <class>) n := n + number-dependent-generics(c) end);
  n
end method;


define method combine-library-results (app-results :: <application-profile-results>)
  let combined-generics = profile-generic-results(app-results);
  when (empty?(combined-generics))
    for (library-results in profile-library-results(app-results))
      for (generic-result keyed-by generic in profile-generic-results(library-results))
        let shared-generic-result
          = (element(combined-generics, generic, default: #f)
               | (element(combined-generics, generic) := make(<generic-profile-result>)));
        for (call-site-result in generic-call-site-profile-results(generic-result))
          add!(generic-call-site-profile-results(shared-generic-result), call-site-result);
          incf(profile-size(shared-generic-result), profile-size(call-site-result));
          incf(profile-hits(shared-generic-result), profile-hits(call-site-result));
          incf(profile-weighted-hits(shared-generic-result), profile-weighted-hits(call-site-result));
          incf(profile-cache-attempts(shared-generic-result), profile-cache-attempts(call-site-result));
          incf(profile-cache-hits(shared-generic-result), profile-cache-hits(call-site-result));
          incf(profile-poly(shared-generic-result), size(call-site-method-profile-results(call-site-result)));
        end for;
      end for;
    end for;
  end when;
end method;

define method print-dispatch-statistics
    (app-results :: <application-profile-results>,
     #key library :: false-or(<symbol>), profile-base :: false-or(<string>),
          full? = #t, by-library? = #f, hits-only? = #t, app-results-only? = #f, uncalled-methods? = #f, app-details? = #t)
  let stream   = #f;
  // HACK: pentium-dw command parser is brain damaged
  when (profile-base)
    let end-index = find-key(profile-base, curry(\==, ' ')) | size(profile-base);
    profile-base := copy-sequence(profile-base, end: end-index);
  end when;
  local method current-stream (app-stream, library) => (stream)
          if (app-stream & app-stream ~== *standard-output*)
            app-stream
          else
            let filename
              = if (library & profile-base)
                  concatenate(profile-base, as(<string>, library), ".prf")
                end if;
            let stream
              = if (filename)
                  make(<file-stream>, locator: filename, direction: #"output", if-exists: #"replace")
                else
                  *standard-output*
                end if;
            stream
          end if
        end method;
  block ()
    local method compare-weights (x-weight :: <profile-weight>, y-weight :: <profile-weight>)
            let x-weighted-hits = profile-weighted-hits(x-weight);
            let y-weighted-hits = profile-weighted-hits(y-weight);
            case
              x-weighted-hits > y-weighted-hits
                => #t;
              x-weighted-hits = y-weighted-hits
                => profile-hits(x-weight) > profile-hits(y-weight);
              otherwise
                => #f;
            end case;
          end method,
          method compare-keyed-weights (weights :: <object-table>, x, y)
            compare-weights(weights[x], weights[y])
          end method,
          method sorted-weight-keys (weights :: <object-table>)
            let keys        = key-sequence(weights);
            let sorted-keys = sort(keys, test: curry(compare-keyed-weights, weights));
            sorted-keys
          end method,
          method save-float-divide (x :: <abstract-integer>, y :: <abstract-integer>)
            if (y = 0)
              0.0
            else
              as(<float>, x) / as(<float>, y)
            end if
          end method,
          method print-size (weight :: <profile-weight-and-size>)
            format(stream, "TOT SIZE %= AVG SIZE %=",
                   profile-size(weight),
                   save-float-divide(profile-size(weight), profile-number-members(weight)))
          end method,
          method print-poly (weight :: <profile-weight-and-size-and-cache-and-poly>)
            format(stream, "POLY %=",
                   save-float-divide(profile-poly(weight), profile-number-call-sites(weight)))
          end method,
          method print-number-calls (weight :: <profile-weight-and-size-and-cache-and-poly>)
            let ct-s-calls = profile-number-static-calls(weight);
            let ct-d-calls = profile-number-dynamic-calls(weight);
            format(stream, "CT-S-CALLS %= CT-D-CALLS %= S/D %=", ct-s-calls, ct-d-calls,
                   save-float-divide(ct-s-calls, ct-d-calls + ct-s-calls));
            local method rt-s-call (weight :: <profile-weight>) => (res :: <integer>)
                    if (profile-hits(weight) > 0 & profile-weighted-hits(weight) = 0)
                      1
                    else
                      0
                    end if
                  end method;
            let rt-s-calls = map-profile-call-sites(rt-s-call, weight);
            let rt-d-calls = map-profile-call-sites(method (x) 1 end, weight);
            format(stream, " RT-S-CALLS %= (%=) E-RT-S/D %=",
                   rt-s-calls, rt-d-calls,
                   save-float-divide(ct-s-calls + rt-s-calls, ct-d-calls + ct-s-calls));
          end method,
          method print-weighted-result (weight :: <profile-weight>, full?)
            format(stream, "%s%= %s%=",
                   if (full?) "HITS " else "" end,
                   profile-hits(weight),
                   if (full?) "COST " else "" end,
                   profile-weighted-hits(weight));
            when (full?)
              format(stream, " COST/HIT %=",
                     save-float-divide(profile-weighted-hits(weight), profile-hits(weight)));
            end when;
          end method,
          method print-weighted-and-cache-result (weight :: <profile-weight-and-size-and-cache>, full?)
            print-weighted-result(weight, full?);
            when (full?)
              format(stream, " C-HITS %= C-TRIES %=",
                     profile-cache-hits(weight),
                     profile-cache-attempts(weight));
              format(stream, " HIT-RATE %=",
                     save-float-divide(profile-cache-hits(weight), profile-cache-attempts(weight)));
            end when;
          end method,
          method print-types (types :: <simple-object-vector>, readable?)
            for (specializer in types, first? = #t then #f)
              unless (first?)
                format(stream, ", ");
              end unless;
              print-specializer(stream, specializer, readable?);
            end for;
          end method,
          method sorted-library-keys (library :: false-or(<symbol>), library-results :: <table>)
            block (return)
              for (lib in key-sequence(library-results))
                when (lib & as(<symbol>, namespace-name(lib)) == library)
                  return(vector(lib))
                end when
              end for;
              sorted-weight-keys(library-results)
            end block;
          end method,
          method print-call-site-results
              (generic :: <generic-function>, call-site-results :: <stretchy-object-vector>,
               seen-methods :: <object-table>)
            for (call-site-result in sort(call-site-results, test: compare-weights))
              when (~hits-only? | (hits-only? & profile-hit?(call-site-result)))
                format(stream, "  ");
                when (full?)
                  format(stream, "CALL-SITE %s ",
                         call-site-library-name(call-site-result));
                end when;
                unless (call-site-id(call-site-result) == -1)
                  format(stream, "%= ", call-site-id(call-site-result));
                end unless;
                when (full?)
                  format(stream, "(");
                  print-types(call-site-types(call-site-result), ~full?);
                  format(stream, ") ");
                  print-size(call-site-result);
                  format(stream, " POLY %d ", size(call-site-method-profile-results(call-site-result)));
                end when;
                print-weighted-and-cache-result(call-site-result, full?);
                format(stream, "\n");
                let method-results = call-site-method-profile-results(call-site-result);
                for (methood in sorted-weight-keys(method-results))
                  seen-methods[methood] := #t;
                  let method-result = method-results[methood];
                  when (~hits-only? | (hits-only? & profile-hit?(method-result)))
                    if (full?)
                      format(stream, "    (");
                      print-types(function-specializers(methood), ~full?);
                      format(stream, ") ");
                    else
                      format(stream, "    %d ", find-key(generic-function-methods(generic), curry(\==, methood)));
                    end if;
                    print-weighted-result(method-result, full?);
                    format(stream, "\n");
                  end when;
                end for;
              end when;
            end for;
          end method,
          method print-generic-results (generic-results :: <table>)
            let seen-methods = make(<table>);
            for (generic in sorted-weight-keys(generic-results))
              let generic-results = generic-results[generic];
              when (~hits-only? | (hits-only? & profile-hit?(generic-results)))
                format(stream, "\n");
                when (full?)
                  format(stream, "%sGENERIC ",
                         if (generic-function-sealed?(generic)) "SEALED " else "OPEN " end);
                end when;
                format(stream, "%s", debug-name(generic));
                when (full?)
                 format(stream, " ");
                  print-size(generic-results);
                  format(stream, " ");
                  print-poly(generic-results);
                end when;
                format(stream, " ");
                print-weighted-and-cache-result(generic-results, full?);
                format(stream, "\n");
                print-call-site-results(generic, generic-call-site-profile-results(generic-results), seen-methods);
                if (full?)
                  unless (hits-only? | ~uncalled-methods?)
                    let uncalled? = #t;
                    for (methood in generic-function-methods(generic))
                      unless (element(seen-methods, methood, default: #f))
                        when (uncalled?)
                          format(stream, "  UNCALLED METHODS\n");
                          uncalled? := #f;
                        end when;
                        format(stream, "    (");
                        print-types(function-specializers(methood), ~full?);
                        format(stream, ")\n");
                      end unless;
                    end for;
                  end unless;
                else
                  format(stream, "  -\n");
                  for (methood in generic-function-methods(generic), i :: <integer> from 0)
                    when (element(seen-methods, methood, default: #f))
                      format(stream, "  %d (", i);
                      print-types(function-specializers(methood), ~full?);
                      format(stream, ")\n");
                    end when;
                  end for;
                end if;
                remove-all-keys!(seen-methods);
              end when;
            end for;
          end method,
          method print-library-results (library, library-results)
            let app-stream = stream;
            stream := #f;
            block ()
              stream := current-stream(app-stream, namespace-name(library));
              format(stream, "LIBRARY %s ", namespace-name(library));
              when (app-details?)
                format(stream, "GENERICS %= ", number-of-generics(library-results));
                print-number-calls(library-results);
                format(stream, " ");
              end when;
              print-poly(library-results);
              format(stream, " ");
              print-size(library-results);
              format(stream, " ");
              print-weighted-and-cache-result(library-results, #t);
              format(stream, "\n");
              print-generic-results(profile-generic-results(library-results));
              format(stream, "\n");
            cleanup
              unless (~stream | stream == app-stream | stream == *standard-output*)
                close(stream)
              end unless;
              stream := app-stream;
            end block;
          end method;
   let library-results = profile-library-results(app-results);
    stream := current-stream(#f, library);
    unless (library)
      unless (by-library? | app-results-only?)
        combine-library-results(app-results);
      end unless;
      format(stream, "APPLICATION ");
      when (app-details?)
        format(stream, "GENERICS %= ", number-of-generics(app-results));
        format(stream, "CLASSES %= DEP-GFS %= ",
               number-of-classes(), number-subclass-dependent-generics());
        print-number-calls(app-results);
        format(stream, " ");
      end when;
      print-poly(app-results);
      format(stream, " ");
      print-size(app-results);
      format(stream, " ");
      print-weighted-and-cache-result(app-results, #t);
      format(stream, "\n");
    end unless;
    unless (app-results-only?)
      format(stream, "\n");
      if (library | by-library?)
        for (library in sorted-library-keys(library, library-results))
          when (library)
            print-library-results(library, library-results[library]);
          end when;
        end for;
      else
        print-generic-results(profile-generic-results(app-results));
      end if;
    end unless;
  cleanup
    unless (stream == *standard-output*)
      close(stream)
    end unless;
  end block;
end method;

define method enable-call-site-caches-only (library)
  call-site-caches-enabled?()          := #t;
  profile-all-terminal-engine-nodes?() := #t;
  decache-all-generics(library);
end method;

define method enable-generic-caches-only (library)
  call-site-caches-enabled?()          := #f;
  profile-all-terminal-engine-nodes?() := #t;
  decache-all-generics(library);
end method;
