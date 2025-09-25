Module:   dfmc-optimization
Synopsis: Assignment elimination & (in the future) optimization.
Author:   Jonathan Bachrach, Keith Playford, Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Eliminate-assignments turns all temporaries that have assignments
// into boxed objects, and replaces all references to those temporaries
// to primitive operations which work on these boxed objects.

// define compilation-pass eliminate-assignments,
//   visit: functions,
//   mandatory?: #t,
//   before: analyze-calls;

define method eliminate-assignments (f :: <&lambda>)
  for (t in f.environment.temporaries)
    if (~empty?(t.assignments) & ~cell?(t))
      cell-assigned-temporaries(t);
    end if;
  end for;
  strip-assignments(environment(f));
end method eliminate-assignments;

define method cell-assigned-temporaries (t :: <temporary>)
  let (make-cell-first-c, make-cell-last-c, cell)
    = convert-make-cell(t.environment, t);
  insert-computations-after!
    (t.generator | t.environment.lambda.body,
     make-cell-first-c, make-cell-last-c);
  for (user in t.users)
    unless (user == make-cell-first-c | user == make-cell-last-c)
      let (get-first-c, get-last-c, get-t)
        = with-parent-computation (user)
            convert-get-cell-value(user.environment, cell);
          end;
      insert-computations-before-reference!
        (user, get-first-c, get-last-c, t);
      replace-temporary-references!(user, t, get-t);
    end unless;
  end for;
  for (assignment in t.assignments)
    assert(assignment.assigned-binding == t);
    let val-t = assignment.computation-value;
    let (set-first-c, set-c, set-t)
      = with-parent-computation (assignment)
          convert-set-cell-value!(assignment.environment, cell, val-t);
        end;
    insert-computations-after!(assignment, set-first-c, set-c);
    replace-temporary-in-users!(assignment.temporary, val-t);
    delete-computation!(assignment);
    // Track cell writes
    cell.assignments := add!(cell.assignments, set-c);
  end for;
  t.assignments := #(); // should this happen automatically?
end method cell-assigned-temporaries;

// Constructors for celling primitives.

/// TODO: SHOULD RESTRICT RAW CELLS TO WHEN THEY DONT CREATE MORE
///       BOX/UNBOX THAN THEY REMOVE...

define method convert-make-cell
    (env :: <lambda-lexical-environment>, t :: <temporary>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <cell>);
  local
    method make-cell
        (env :: <lambda-lexical-environment>, t :: <temporary>)
     => (first-c :: <computation>, last-c :: <computation>, t :: <cell>);
      let type
        = as(<&type>, type-estimate(t));
      let (unboxer-c, unboxed-t)
        = maybe-convert-unbox(env, t, type);
      let (c, tmp)
        = make-with-temporary
            (env, <make-cell>, value: unboxed-t, temporary-class: <cell>);
      let cell = c.temporary;
      cell-type(cell) := type;
      rename-temporary!(t, cell);
      join-1x1-t!(unboxer-c, c, tmp)
    end method;
  if (t.generator)
    with-parent-computation (t.generator)
      make-cell(env, t)
    end
  else
    // No geneator, probably an argument
    with-parent-source-location (env.lambda.lambda-source-location)
      make-cell(env, t)
    end
  end if;
end method convert-make-cell;

define method convert-get-cell-value
    (env :: <lambda-lexical-environment>, cell :: <cell>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <temporary>)
  let (c, tmp) =
    make-with-temporary(env, <get-cell-value>, cell: cell);
  let (boxer-c, boxed-t)
    = maybe-convert-box(env, tmp, cell-type(cell));
  join-1x1-t!(c, boxer-c, boxed-t);
end method convert-get-cell-value;

define method convert-set-cell-value!
    (env :: <lambda-lexical-environment>, cell :: <cell>,
     ref :: <value-reference>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <temporary>)
  let (unboxer-c, unboxed-t)
    = maybe-convert-unbox(env, ref, cell-type(cell));
  let (c, tmp) =
    make-with-temporary
      (env, <set-cell-value!>, cell: cell, value: unboxed-t);
  join-1x1-t!(unboxer-c, c, tmp);
end method convert-set-cell-value!;

// Once boxing has been completed we introduce additional temporaries in the
// consequent branch of conditionals to improve typing precision.
// We do this when the conditional has the following form:
//
//    if (x) ...
//
// In this case we introduce a new temporary for the duration of the
// consequent branch whose type is the inferred type for x with any #f
// component removed.
//
// The temporaries are introduced via a new kind of temporary-transfer
// computation called <constrain-type>.  The type component of this
// computation has two roles corresponding to the two scenarios outlined above
// (i.e. it is either #f or contains a type).
// Due to limitations of the type system (e.g. no intersection types) the type
// pruning is only an approximation.  However, this should always be safe as
// the worst that will happen is that the new temporary will have the same type
// as the original variable.

define method type-minus-false (te :: <type-estimate>) => (type == #f)
  #f
end method;

define method type-minus-false
    (te :: <type-estimate-union>) => (type :: false-or(<value-reference>))
  let found-false?
    = #f;
  let false-te
    = make-type-estimate(<type-estimate-limited-instance>, singleton: #f);
  let new-unionees
    = collecting ()
        for (te in type-estimate-unionees(te))
          if (type-estimate-subtype?(te, false-te))
            found-false? := #t;
          else
            collect(te)
          end if
        end for;
      end collecting;
  if (found-false? & size(new-unionees) = 1)
    let te = first(new-unionees);
    if (instance?(te, <type-estimate-class>))
      make-object-reference(as(<&type>, te))
    else
      #f
    end if
  else
    #f
  end if;
end method;

define method maybe-rename-temporaries-in-conditional
    (c :: <if>) => (b :: <boolean>)
  let test-reference = c.test;
  let test-temporary
    = instance?(test-reference, <temporary>)
    & test-reference;
  let te = type-estimate(test-temporary);
  let constraint
    = test-temporary
    & type-minus-false(te);
  if (constraint) // So we need to introduce a new temporary
    with-parent-computation (c)
      let (tt-c, tt-t) =
        make-with-temporary
          (c.environment, <constrain-type>,
           value: test-temporary, type: constraint);
      let then-f = c.consequent;
      let merge-c :: <if-merge> = c.next-computation;
      let changed? = #f;
      rename-temporary!(test-temporary, tt-t);
      for-computations(tc from then-f before merge-c)
        let now-changed?
          = if (instance?(tc, <constrain-type>))
              tc.type := constraint;
              #f
            else
              rename-temporary-references!(tc, test-temporary, tt-t)
            end if;
        changed? := (now-changed? | changed?);
      end;
      // The left side of the merge is also part of the consequent
      if (merge-c.merge-left-value == test-temporary)
        merge-replace-left-value!(merge-c, test-temporary, tt-t);
        changed? := #t;
      end;
      if (changed?)
        insert-computation-before-reference!(then-f, tt-c, tt-t);
      else // It's not used in the consequent, so get rid of it.
        remove-user!(test-temporary, tt-c);
      end;
      changed?
    end
  end
end method;
