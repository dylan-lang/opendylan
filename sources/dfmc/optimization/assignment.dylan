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
   with-parent-computation (t.generator)
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
     join-1x1-t!(unboxer-c, c, tmp);
   end;
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
// We do this when the conditional has one of the following forms:
//
//    if (x) ...                         if (instance?(x, y)) ...
//
// In the first case we introduce a new temporary for the duration of the
// consequent branch whose type is the inferred type for x with any #f
// component removed.
// In the second case the temporary's type is the intersection of the type
// of x and y.  We only do this transformation for unboxed variables.
//
// The temporaries are introduced via a new kind of temporary-transfer
// computation called <constrain-type>.  The type component of this
// computation has two roles corresponding to the two scenarios outlined above
// (i.e. it is either #f or contains a type).
// Due to limitations of the type system (e.g. no intersection types) the type
// pruning is only an approximation.  However, this should always be safe as
// the worst that will happen is that the new temporary will have the same type
// as the original variable.

define method maybe-rename-temporaries-in-conditionals (f :: <&lambda>)
  for-computations (c in f)
    maybe-rename-temporaries-in-conditional(c, f)
  end;
end;

define method maybe-rename-temporaries-in-conditional
  (c :: <computation>, f :: <&lambda>)
end;

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
  (c :: <if>, f :: <&lambda>)
  let gen-arg0 = c.test;
  let (rename?, to-be-renamed, constraint) =
    if (instance?(gen-arg0, <lexical-variable>))
      values(#t, gen-arg0, type-minus-false(type-estimate(gen-arg0)))
    elseif (instance?(gen-arg0, <temporary>))
      let gen-gen = gen-arg0.generator;
      if (instance?(gen-gen, <simple-call>))
        let (constant?, value) = fast-constant-value?(gen-gen.function);
        if (constant? & (value == dylan-value(#"instance?"))
              & size(gen-gen.arguments) > 1)   // so that can do [1]
          let (constant-type?, constant-type) =
            fast-constant-value?(gen-gen.arguments[1]);
          if (constant-type?)
            values(#t, gen-gen.arguments[0], gen-gen.arguments[1])
          end;
        end;
      end;
    end;
  if (rename?) // So we need to introduce a new temporary
    let (tt-c, tt-t) =
      make-with-temporary
        (c.environment, <constrain-type>,
         value: to-be-renamed, type: constraint);
    let then-f = c.consequent;
    let changed? = #f;
    rename-temporary!(to-be-renamed, tt-t);
    for-computations(tc from then-f before c.next-computation)
      let now-changed? = rename-temporary-references!(tc, to-be-renamed, tt-t);
      changed? := (changed? | now-changed?);
      // was: changed? := (changed? | rename-temporary-references!(tc, to-be-renamed, tt-t));
    end;
    if (changed?)
      insert-computation-before!(then-f, tt-c);
    else // It's not used in the consequent, so get rid of it.
      remove-user!(to-be-renamed, tt-c);
    end
  end;
end method;
