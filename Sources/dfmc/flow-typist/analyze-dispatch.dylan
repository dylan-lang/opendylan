Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <analysis-results> (<object>)
  slot locals-seen = make(<table>);
  slot unique-methods = make(<table>);
  slot folded = make(<table>);
  slot open-gfs = make(<table>);
  slot multiple-methods = make(<table>);
  slot not-guaranteed-within-sealed-domain = make(<table>);
  slot not-guaranteed-applicable = make(<table>);
  slot no-applicable-methods = make(<table>);
  slot possible-dispatch-failures = make(<table>);
  slot no-known-methods = make(<table>);
  slot next-methods = make(<table>);
  slot mega-args = make(<table>);
  slot bottom-in-args = make(<table>);
  slot total-calls = 0;
end;

define method print-object (obj :: <analysis-results>, stream)
  let total = obj.total-calls;
  let constant-folded = obj.folded.size;
  let dispatched = obj.unique-methods.size;
  format(stream, "\nTotal GF calls = %s", total);
  format(stream, "\nConstant  Folded = %s", constant-folded);
  format(stream, "\nCompile Time Dispatches = %s", dispatched);
  unless (total = 0) 
    format(stream, "\nCompile Time Dispatches and Foldings as %% of total: %s\n", 
           (as(<float>, dispatched) + constant-folded)/ total);
  end;
  format(stream, "\nBreakdown of calls which failed to dispatch at compile time.");
  format(stream, "\n------------------------------------------------------------\n");
  format(stream, "\nCalls to completely Unsealed GFs = %s", obj.open-gfs.size);
  format(stream, "\nCalls where inferred union arg types have multiple applicable methods = %s", 
                 obj.multiple-methods.size);
  format(stream, "\nFailed calls to GFs with sealed domains = %s", obj.not-guaranteed-within-sealed-domain.size);
  format(stream, "\nCalls where inferred arg types are not precise enough to select any method in a sealed GF = %s", 
                 obj.not-guaranteed-applicable.size);
  format(stream, "\nFailures due to next method call = %s", obj.next-methods.size);
  format(stream, "\nFailures due to Megamorphism in inferred arg types = %s", obj.mega-args.size);
  format(stream, "\n(Current Megamorphic threshold = %s)", $mega-morphic-threshold);
  format(stream, "\nCalls which received <bottom> as an argument = %s", obj.bottom-in-args.size);
  format(stream, "\nCalls with no applicable methods = %s", obj.no-applicable-methods.size);
  format(stream, "\nCalls which may fail due to ambiguity= %s", obj.possible-dispatch-failures.size);
end;

define method record-call-result (analysis-results, result)
end;

define method record-call-result 
  (analysis-results, result :: <unique-method>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.unique-methods[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <applied-method-record>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.unique-methods[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <generic-constant-folded>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.folded[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <next-method-called>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.next-method-list-not-guaranteed-joint[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <open-gf-no-sealing>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.open-gfs[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <open-gf-sealed-domains>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.not-guaranteed-within-sealed-domain[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <lack-of-methods>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.no-applicable-methods[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <imprecise-argument-types>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.not-guaranteed-applicable[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <union-argument-types>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.multiple-methods[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <bottom-in-arg-list>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.bottom-in-args[result] := result;
end;

define method record-call-result 
  (analysis-results, result :: <ambiguous-dispatch>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.possible-dispatch-failures[result] := result;
end;

define method record-call-result (analysis-results, result :: <megamorphic-args>)
  *analysis-results*.total-calls :=   *analysis-results*.total-calls + 1;
  analysis-results.mega-args[result] := result;
end;




define variable *analysis-results* = #f;

define function analyze-lib(lib)
  *analysis-results* := make(<analysis-results>);
  let l = lookup-library-description (lib);
  with-library-context (l)
    for (comp-rec in library-description-compilation-records(l))
      for (tlf in private-compilation-record-top-level-forms(comp-rec))
        dynamic-bind (*print-method-bodies?* = #f)
          analyze-form(tlf);
        end;
      end;
    end;
  end;
  *analysis-results*
end;

define method analyze-form(o)
end;

define method analyze-form
    (form :: <binding-defining-form>) => ();
  with-dependent($compilation of form)
    when (form-init-method(form))
      analyze-method(form-init-method(form));
    end;
  end;
end;

define method analyze-form (o :: <method-defining-form>)
  analyze-method(form-model(o));
end;

define method analyze-form (form :: <constant-method-definition>)
  with-dependent($compilation of form)
    if (form-init-method(form))
      analyze-method(form-init-method(form));
    end;
  end;
  analyze-method(form-model(form));
end;

define method analyze-method(m)
end;

define method analyze-method(l :: <&lambda>)
  let css = get-default-call-site-summary(l);
    if (css.result-type == typist-<unknown-type>())
      format-out("analyze-method %=\n", css);
    end;
  unless (element(*analysis-results*.locals-seen, css, default: #f))
    *analysis-results*.locals-seen[css] := #t;
 
    for (result in css.computation-records)
      record-call-result(*analysis-results*, result);
    end;
    for (summary in css.callees)
        if (~lambda-top-level?(summary.lambda))
          analyze-css(summary);
        end;
    end;
  end;
end;

define method analyze-css(css :: <call-site-summary>)
  unless (element(*analysis-results*.locals-seen, css, default: #f))
    *analysis-results*.locals-seen[css] := #t;
    for (result in css.computation-records)
      record-call-result(*analysis-results*, result);
    end;
    for (summary in css.callees)
        if (~lambda-top-level?(summary.lambda))
          analyze-css(summary);
        end;
    end;
  end;
end;


define method analyze-argument-type-imprecision 
  (analysis-result, #key stream = *standard-output*)
  let calls = analysis-result.not-guaranteed-applicable;
  let result = make(<table>);
  local method classify (x)
          let gf-info = element(result, x.called-function, default: #f);
          element(result, x.called-function) := 
          if (gf-info)
            pair(x, gf-info);
          else
            list(x);
          end;
        end,
        method test (x, y) x.size > y.size end;
  
  map(classify, calls);

  let sorted-result = sort(as(<vector>, result), test: test);

  format(stream, 
         "Dispatch failures due to arg type imprecision given sealed GFs\n");
  format(stream, 
         "------------------------------------------------------------------\n");

  for (x in sorted-result)
    format(stream, "\n%s  = %s",
            x.head.called-function.^debug-name, x.size);
  end;
  *sorted-result* := sorted-result;
  #f;
end;


define method analyze-failed-dispatch-potential-methods
  (analysis-result, #key stream = *standard-output*)
  let calls = analysis-result.not-guaranteed-applicable;
  let result = make(<table>);
  let failed-dispatches = make(<table>);
  local method classify (x)
          let methods = union(x.potentially-applicable-methods, map(lambda, x.call-site-summaries));
          let n = methods.size;
          let m = element(result, n, default: 0);
          result[n] := m + 1;
          failed-dispatches[n] := pair(x, element(failed-dispatches, n, default: #()));
        end;
  
  map(classify, calls);

  *failed-dispatches* := failed-dispatches;
  format(stream, 
         "Number of potential methods in non-dispatched sealed GF calls\n");
  format(stream, 
         "------------------------------------------------------------------\n");
  
  for (x in sort(key-sequence(result), test: \<))
    format(stream, "\n%s  = %s", x, result[x]);
  end;
end;


define method analyze-sealed-domain-failures
  (analysis-result, #key stream = *standard-output*)
  let calls = analysis-result.not-guaranteed-within-sealed-domain;
  let result = make(<table>);
  local method classify (x)
          result[x.called-function] := 
            pair(x, element(result, x.called-function, default: #()));
        end,
        method test (x, y) x.size > y.size end;
  
  map(classify, calls);

  let sorted-result = sort(as(<vector>, result), test: test);

  *sealed-domain-failures* := sorted-result;
  format(stream, 
         "Dispatch failures due to arg type imprecision given sealed domains\n");
  format(stream, 
         "------------------------------------------------------------------\n");
  for (x in sorted-result)
    format(stream, "\n%s  = %s",
            x.head.called-function.^debug-name, x.size);
  end;
  
end;

define method analyze-failed-dispatch-integer
  (analysis-result, #key stream = *standard-output*)
  let l = lookup-library-description (#"dylan");
  with-library-context (l)
    without-dependency-tracking

      let calls1 = analysis-result.not-guaranteed-applicable;
      let calls2 = analysis-result.not-guaranteed-within-sealed-domain;
      let int = dylan-value(#"<integer>");
      let small-int = dylan-value(#"<integer>");
      let result = make(<table>);
      local method classify (x)
              let k = find-key(x.input-types, curry(\==, int));
              when (k)
                let methods = if (instance?(x, <open-gf-sealed-domains>))
                                x.called-function.^generic-function-methods-known;
                              else
                                union(x.potentially-applicable-methods, map(lambda, x.call-site-summaries));
                              end;
                unless (empty?(methods))
                  let limit = methods[0].^function-signature.^signature-number-required;
                  when (k < limit)
                    block (exit)
                      for (m in methods)
                        when (m.^function-signature.^signature-required[k] == small-int)
                          result[x] := m;
                          exit();
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
  
      map(classify, calls1);
      map(classify, calls2);

      format(stream, 
             "Number of possible dispatch failures due to <INTEGER> = %s\n", result.size);
      result;
    end;
  end;
end;

define variable *dispatch-analysis* = #f;
define variable *seen* = #f;

define function analyze-lib-dispatches(lib)
  *dispatch-analysis* := make(<table>);
  *seen* := make(<table>);
  let l = lookup-library-description (lib);
  with-library-context (l)
    for (comp-rec in library-description-compilation-records(l))
      for (tlf in private-compilation-record-top-level-forms(comp-rec))
        dynamic-bind (*print-method-bodies?* = #f)
          analyze-dispatches(tlf);
        end;
      end;
    end;
  end;
  *seen* := #f;
  massage-dispatch-results(*dispatch-analysis*);
end;

define method analyze-dispatches(o)
end;

define method analyze-dispatches
    (form :: <binding-defining-form>) => ();
  with-dependent($compilation of form)
    when (form-init-method(form))
      analyze-method(form-init-method(form));
    end;
  end;
end;

define method analyze-dispatches (o :: <method-defining-form>)
  analyze-dispatches(form-model(o));
end;

define method analyze-dispatches (form :: <constant-method-definition>)
  with-dependent($compilation of form)
    if (form-init-method(form))
      analyze-dispatches(form-init-method(form));
    end;
  end;
  analyze-dispatches(form-model(form));
end;


define method analyze-dispatches(l :: <&lambda>)
  let css = get-default-call-site-summary(l);
    if (css.result-type == typist-<unknown-type>())
      format-out("analyze-dispatches %=\n", css);
    end;
  unless (element(*seen*, css, default: #f))
    element(*seen*, css) := #t;
    let analysis-result = make(<analysis-results>);
    analysis-result.total-calls :=   css.computation-records.size;
    for (result in css.computation-records)
      record-call-result(analysis-result, result);
    end;
    for (summary in css.callees)
      unless (lambda-top-level?(summary.lambda))
        analyze-css-dispatches(summary, analysis-result);
      end;
    end;
    *dispatch-analysis*[l] := analysis-result;
  end;
end;

define method analyze-css-dispatches(css, analysis-result)
  unless (element(*seen*, css, default: #f))
    element(*seen*, css) := #t;
    analysis-result.total-calls := analysis-result.total-calls + css.computation-records.size;
    for (result in css.computation-records)
      record-call-result(analysis-result, result);
    end;
    for (summary in css.callees)
      unless (lambda-top-level?(summary.lambda))
        analyze-css-dispatches(summary, analysis-result);
      end;
    end;
  end;
end;

define function massage-dispatch-results(tab)
  let tab2 = make(<table>);
  for (k in key-sequence(tab))
     tab2[tab[k]] := k;
  end;

  let l = as(<list>, tab);
  local method test (x, y)
          let totalx = x.total-calls;
          let dispatchedx = x.unique-methods.size;
          let totaly = y.total-calls;
          let dispatchedy = y.unique-methods.size;
          let xt = if (totalx = 0) 0.0 else dispatchedx / as(<float>, totalx) end;
          let yt = if (totaly = 0) 0.0 else dispatchedy / as(<float>, totaly)  end;
          xt < yt;
        end;

  let sorted-l = sort(l, test: test);

  collecting (results)
    for (r in sorted-l)
      unless (r.total-calls = 0)
        collect-into(results,pair(tab2[r], r));
      end;
    end;
    *dispatch-analysis* := collected(results);
    dynamic-bind (*print-method-bodies?* = #f)
      for (x in *dispatch-analysis*)
        format-out("\n%s", x.head);
        format-out("\n  %s", x.tail.unique-methods.size / as (<float>, x.tail.total-calls));
      end;
    end;
  end;
end;
  
define function partition-result (f)
  collecting(l)
    for (x in *dispatch-analysis*)
      when ((x.tail.unique-methods.size / as (<float>, x.tail.total-calls)) <= f)
        collect-into(l, x);
      end;
    end;
    *result* := collected(l);
    *result*.size;
  end;
end;

define variable *dispatch-analysis* = #f;
define variable *seen* = #f;

