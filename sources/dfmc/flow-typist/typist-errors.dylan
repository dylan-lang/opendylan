Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// type check computations

define program-warning <run-time-type-error>
  slot condition-inferred-type,
    required-init-keyword: inferred-type:;
  slot condition-expected-type,
    required-init-keyword: expected-type:;
  format-string 
    "Type check will fail - %s inferred, %s expected.";
  format-arguments
    inferred-type, expected-type;
end program-warning;

define program-warning <run-time-result-type-error>
    (<run-time-type-error>)
  format-string 
    "Result type check will fail - %s inferred, %s expected.";
end program-warning;

define thread variable *warn-for-all-summaries* = #f;

define program-warning <possible-run-time-type-error>
    (<run-time-type-error>)
  slot default-path,
    required-init-keyword: default-path:;
  format-string 
    "Type check will fail - %s inferred, %s expected, when called in the context %=.";
  format-arguments
    inferred-type, expected-type, default-path;
end program-warning;

define constant $arrow-ppml-separator =
  vector(ppml-break(space: 1), ppml-string("->"), ppml-break(space: 1));

define method find-default-css-caller(css :: <call-site-summary>)
/*
  let caller-path = #f;
  block (return)  
    local method search(css, path)
            let new-path = pair(css, path);
            if (instance?(css, <default-call-site-summary>))
              unless (caller-path & (caller-path.size < new-path.size))
                caller-path := new-path
              end
	    elseif (~member?(css, path))
	      for (caller in css.callers) search(caller, new-path) end
            end
          end;
    for (caller in css.callers) search(caller, list(css)) end;
  end;
  let path = caller-path | list(css);

  ppml-separator-block(
    map(method (css) 
          ppml-block(vector(ppml-string(as(<string>, css.css-lambda.^debug-name)),
                            ppml-break(space: 0),
                            ppml-separator-block(map(curry(as, <ppml>), css.arg-types),
                              left-bracket: ppml-string("("),          
                              right-bracket: ppml-string(")"))))
        end, path),
    separator: $arrow-ppml-separator)
*/
  ppml-string("{ unknown context }")
end;



define program-warning <non-sequence-last-argument-in-apply>
  slot condition-type-estimate,
    required-init-keyword: type-estimate:;
  format-string 
    "Last argument in apply call is not a sequence - inferred type is %=.";
  format-arguments type-estimate;
end program-warning;

define program-warning <non-function-in-call>
  slot condition-type-estimate,
    required-init-keyword: type-estimate:;
  format-string 
    "Function value in call is not a function - inferred type is %s.";
  format-arguments type-estimate;
end program-warning;

define program-warning <non-function-in-apply-call>
  slot condition-type-estimate,
    required-init-keyword: type-estimate:;
  format-string 
    "Function value in apply call is not a function - inferred type is %s.";
  format-arguments type-estimate;
end program-warning;

define serious-program-warning <incompatible-call>
  slot condition-function,
    required-init-keyword: function:;
end serious-program-warning;

// TODO: Gross hack. What should really be passed? We need something like
// a <function-id>/<object-id>.

define method initialize (c :: <incompatible-call>, #key)
  next-method();
  let def = c.condition-function.model-definition;
  if (def)
    let names = form-variable-names(def);
    c.condition-function 
      := if (names.size = 1) names.first else def end;
  end;
end method;

define program-warning <unknown-keyword-in-call> (<incompatible-call>)
  slot condition-known-keywords,
    required-init-keyword: known-keywords:;
  slot condition-supplied-keyword,
    required-init-keyword: supplied-keyword:;
  format-string 
    "Unknown keyword in call to %s - %s supplied, %s expected.";
  format-arguments 
    function, supplied-keyword, known-keywords;
end program-warning;

define program-warning <argument-count-mismatch-in-call> (<incompatible-call>)
  slot condition-supplied-count,
    required-init-keyword: supplied-count:;
  slot condition-required-count,
    required-init-keyword: required-count:;
  format-arguments function, supplied-count, required-count;
end program-warning;

define program-warning <too-few-arguments-in-call> 
    (<argument-count-mismatch-in-call>)
  format-string "Too few arguments in call to %s - %s supplied, %s expected.";
end program-warning;

define program-warning <too-many-arguments-in-call> 
    (<argument-count-mismatch-in-call>)
  format-string "Too many arguments in call to %s - %s supplied, %s expected.";
end program-warning;

define program-warning <unbalanced-keyword-arguments-in-call>
    (<incompatible-call>)
  slot condition-keyword-supplied-count,
    required-init-keyword: keyword-supplied-count:;
  format-string "Unbalanced keyword arguments in call to %s.";
  format-arguments function, keyword-supplied-count;
end program-warning;

define program-warning <non-keywords-in-call> (<incompatible-call>)
  slot condition-supplied-keyword-type-estimates,
    required-init-keyword: supplied-keyword-type-estimates:;
  format-string 
    "Non-symbol keyword arguments in call to %s - inferred types are %s.";
  format-arguments 
    function, supplied-keyword-type-estimates;
end program-warning;

// gts, hack: change from "type" to "types" to distinguish from the 
// warning in dfmc-optimization/dispatch.dylan
// TODO: merge these to warnings together.

define program-warning <argument-types-mismatch-in-call> (<incompatible-call>)
  slot condition-required-types,
    required-init-keyword: required-types:;
  slot condition-supplied-type-estimates,
    required-init-keyword: supplied-type-estimates:;
  format-string 
    "Invalid argument types in call to %s - %s supplied, %s expected.";
  format-arguments 
    function, supplied-type-estimates, required-types;
end program-warning;

define program-warning <values-argument-types-mismatch-in-call> 
    (<incompatible-call>)
  slot condition-required-types,
    required-init-keyword: required-types:;
  slot condition-supplied-type-estimate,
    required-init-keyword: supplied-type-estimate:;
  format-string 
    "Invalid #rest values in multiple-value call to %s - "
    "#rest %s supplied, %s expected.";
  format-arguments 
    function, supplied-type-estimate, required-types;
end program-warning;

define program-warning <unrecognized-keyword-arguments-in-call>
    (<incompatible-call>)
  slot condition-supplied-keywords,
    required-init-keyword: supplied-keywords:;
  slot condition-recognized-keywords,
    required-init-keyword: recognized-keywords:;
  format-string "Unrecognized keyword arguments in call to %s - "
                "%s unrecognized, %s allowed.";
  format-arguments function, supplied-keywords, recognized-keywords;
end program-warning;

define program-warning <too-many-arguments-in-apply-call> 
    (<argument-count-mismatch-in-call>)
  format-string 
    "Too many arguments in application of %s - "
    "%s supplied positionally to apply, only %s expected.";
end program-warning;

define program-warning <argument-type-mismatch-in-apply-call> 
    (<incompatible-call>)
  slot condition-required-types,
    required-init-keyword: required-types:;
  slot condition-supplied-type-estimates,
    required-init-keyword: supplied-type-estimates:;
  format-string 
    "Invalid argument types in application of %s - "
    "%s supplied positionally to apply, %s expected in the corresponding "
    "positions.";
  format-arguments 
    function, supplied-type-estimates, required-types;
end program-warning;

define program-warning <no-applicable-methods-in-call> (<incompatible-call>)
  slot condition-supplied-type-estimates,
    required-init-keyword: supplied-type-estimates:;
  format-string 
    "No applicable methods for call to %s - inferred argument types %s.";
  format-arguments 
    function, supplied-type-estimates;
end program-warning;






/*
define thread variable *outside-compiler?* = #f;

define function typist-note (#rest args)
  if (/* *outside-compiler?* */ #f)
    format-out("\n%s\n", apply(make, args));
  else
    apply(note, args);
  end;
end;
*/


define function typist-note(class :: <class>, #rest args)
  let source-location-key-index = find-key(args, curry(\==, source-location:));
  let source-location = args[source-location-key-index + 1];

  let lib = current-library-description();
  let current-dependent = *current-dependent*;
  let creator = (current-dependent ~== $no-dependent) & current-dependent;
  let table = lib.library-conditions-table;
  let q = element(table, creator, default: not-found());

  if (found?(q)) 
    block (return)
      for (c in q)
        if ( c.object-class == class
           & source-location
           & c.condition-source-location = source-location)
          return(#f)
        end
      end;

      apply(note, class, args);
    end block;
  else
    apply(note, class, args);
  end
end;

/*
Some typist errors need to be delayed until we have finished typing
the computation.  For example, as the type for a binding may grow
during typing, a guaranteed-disjoint test may initially return #t, but
then later return #f for a binding.  The approach we adopt here is to
use a delated-typist-note function that initially just records the
fact that there was a potential problem in the computation-records
table.  As the context already provides the summary and computation,
the actual entry in the table is unimportant as long as it is unique.
So we make a special computation-record object that we use for this
purpose.  After we are sure that the typing of the computation has
stabilised, we go through the computation records looking for this
distinguished entry.  Whenever we find one we retype the associated
computation.  If this calls delayed-typist-note then this time the note
is actually raised, and hence displayed.

For now we also treat instances of <illegal-call-record> and 
<no-applicable-methods> in this way as well.  This all needs to be tidied up.
*/

define class <delayed-typist-note> (<computation-record>) 
end;

define variable $delayed-typist-note = #f; // Initialize lazily...
  
define thread variable *display-delayed-typist-notes?* = #f;

define method delayed-typist-note
    (css, computation, class :: <class>, 
     #rest args, #key computation-record = #f, #all-keys)

  if (*display-delayed-typist-notes?*)
    apply(typist-note, class, args);
  else
    let comp-rec = 
      computation-record
      | $delayed-typist-note 
      | ($delayed-typist-note := 
           create-computation-record(<delayed-typist-note>, #f, #f, #()));
    record-computation-record(css, computation, comp-rec)
  end
end;

define method process-delayed-typist-notes(css :: <call-site-summary>)
  unless (css.compressed?)
    dynamic-bind (*display-delayed-typist-notes?* = #t)
      let work-agenda = make(<work-agenda>);
      for (comp-rec keyed-by comp in css.computation-records)
        if (comp-rec == $delayed-typist-note)
          refine-initial-node-type(comp, css, work-agenda);
          css.computation-records[comp] := #f;
        elseif ( instance?(comp-rec, <illegal-call-record>)
               | instance?(comp-rec, <no-applicable-methods>) )
          refine-initial-node-type(comp, css, work-agenda);
        end
      end
    end
  end
end;

define method process-delayed-typist-notes(lambda :: <&lambda>)
  if (*warn-for-all-summaries*)
    for (css in lambda.call-site-summaries)
      process-delayed-typist-notes(css)
    end
  else
    process-delayed-typist-notes(get-default-call-site-summary(lambda))
  end
end;












