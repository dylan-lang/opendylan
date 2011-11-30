Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Algebra of the types in the typist.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// Implementation of the typist algebra.  (NB: No macro-defining macros!)
///

///
/// Normalization of <type-estimate>s.
///
/// Any <type-estimate> which contains other <type-estimate>s (i.e., "type
/// constructors") is subject to normalization.  The theory of normal form here
/// is disjunctive normal form, i.e., pull all the unions as far outside as 
/// they'll come.  Sort of like prenex/prolix form in logic.  
///
/// See Agesen's CPA paper: Agesen, O., "The Cartesian Product Algorithm: Simple
/// and Precise Type Inference of Parametric Polymorphism," ECOOP-95, 
/// http://self.smli.com/papers/cpa.html.
///

/* 
// *** This is debugging code, normally dead.
define constant $normalize-freqs$ = make(<table>);

// *** This is debugging code, normally dead.
define function bump-normalization-freq(cl :: <class>) => ()
  $normalize-freqs$[cl] := element($normalize-freqs$, cl, default: 0) + 1;
  values()
end;

// *** This is debugging code, normally dead.
define function show-normalize-stats ()
  // See what crawls out when we lift the rocks.
  format-out("\nType Normalization Statistics.\n==============================\n");
  let total = 0;
  let alist = #();
  map-table($normalize-freqs$,
            method (cl :: <class>, freq :: <integer>)
              // Accumulate total counts, and freq/cl pairs for sorting.
              alist := pair(pair(freq, cl), alist);
              total := total + freq
            end);
  // Sort in order of decreasing frequency for presentation to programmer.
  alist := sort!(alist, test: method (x, y) first(x) > first(y) end);
  for (cell in alist)
    let freq = first(cell);
    let cl   = rest(cell);
    format-out("\n%s\t%d", cl, freq)
  end;
  format-out("\n\nTotal calls: %d", total);
  values()
end;
*/

// A "megamorphic" type is one that is (unreasonably) extremely polymorphic,
// e.g., a union of inconveniently large size.  These actually occur in 
// practice: the Win32 library has a create-font function of 14 arguments, 
// each of which is a union (<integer>, <machine-word>); that would lead
// to a CPA expansion of 2^14 elements!

// Somewhat arbitrary, but bigger arbitrary numbers screw us compiling the
// FFI.
define constant $megamorphic-punt-threshold$ = 32; 

// *** This probably conses too much!
define function type-estimate-CP-expand(megamorphic-punt-constr :: <function>,
                                        constr                  :: <function>, 
                                        #rest products)
  => (CP-expansion :: <type-estimate>)
  // Cartesian Product expansion.  Products is a list of args to be 
  // CP-expanded.  Constr gets called on the final unionees to build the 
  // type you want to be the element of the final union.
  local method do-CP (fn :: <function>, sets :: <list>) => ()
          // Do fn over the Cartesian product of sets.  Sets is a <list> of
          // <sequence>s; each <sequence> represents a set (ordered, of course).
          // It's a <list> so we can tail() it in the recursive case.
          case
            empty?(sets)       => ;                   // 0 sets: do nothing
            empty?(tail(sets)) => do(fn, head(sets)); // 1 set: do elements
            otherwise          => do(method (x)       // 2 or more sets: recurse
                                       do-CP(curry(fn, x), tail(sets))
                                     end,
                                     head(sets));
          end;
          values()
        end,
        method as-union-list (x) => (union-list :: <sequence>)
          // Convert unions to lists of unionees, others to list of self.
          select (x by instance?)
            <type-estimate-union> => type-estimate-unionees(x);
            otherwise             => list(x);
          end
        end,
        method final-union-size() => (union-size :: <integer>)
          // How big is this gonna be, Batman?
          reduce(method (so-far, product)
                   so-far * if (instance?(product, <type-estimate-union>))
                              size(type-estimate-unionees(product))
                            else
                              1
                            end
                 end,
                 1, 
                 products)
        end;
  if (final-union-size() > $megamorphic-punt-threshold$)
    // Unreasonably large union would be made from this extremely polymorphic 
    // type; punt to something else instead.  Caller defines "something else."
    megamorphic-punt-constr()
  else
    // Do regular CPA expansion.
    let CP-unionees :: <object-deque> = make(<object-deque>);
    do-CP(method (#rest args)
            // Call the constructor args.  Result is an element of CP union.
            push-last(CP-unionees, apply(constr, args))
          end,
          map-as(<list>, as-union-list, products));
    // Will be automatically normalized in case the union is degenerate.
    make(<type-estimate-union>, unionees: CP-unionees)
  end
end;

define method type-estimate-normalize(bot :: <type-estimate-bottom>)
 => (te :: <type-estimate-bottom>)
  // Bottom normalization is trivial.
  // bump-normalization-freq(object-class(bot));
  type-estimate-to-be-normalized?(bot) := #f;
  bot
end;

define method type-estimate-normalize(top :: <type-estimate-top>)
 => (te :: <type-estimate-top>)
  // Top normalization.
  // bump-normalization-freq(object-class(top));
  type-estimate-to-be-normalized?(top) := #f;
  top
end;

define method type-estimate-normalize(lim :: <type-estimate-limited>)
 => (te :: <type-estimate-limited>)
  // Some limited normalization is trivial.
  // Limited integers, limited classes, limited collections & limited functions
  // override this, below.  Limited instances inherit it.
  // bump-normalization-freq(object-class(lim));
  type-estimate-to-be-normalized?(lim) := #f;
  lim
end;

define method type-estimate-normalize(li :: <type-estimate-limited-integer>)
 => (te :: <type-estimate-limited>)
  // Override method for limited integers.
  // bump-normalization-freq(object-class(li));
  if (~type-estimate-to-be-normalized?(li))
    // Already been normalized
    li
  elseif (type-estimate-min(li) = type-estimate-max(li))
    // "Severely limited" integers are of size one -- normalize to singletons.
    make(<type-estimate-limited-instance>, singleton: type-estimate-min(li))
  else
    // Just note that it's normalized & return it.
    type-estimate-to-be-normalized?(li) := #f;
    li
  end
end;

define method type-estimate-normalize (lc :: <type-estimate-limited-class>)
 => (te :: <type-estimate-limited>)
  // Override method for limited classes.
  // bump-normalization-freq(object-class(lc));
  let sub-cl = type-estimate-subclass(lc);
  if (~type-estimate-to-be-normalized?(lc))
    // Already been normalized
    lc
  elseif (^sealed-with-no-subclasses?(sub-cl))
    // If a class is sealed & has no subclasses in this library, it will never
    // have any.  Such "severely limited" classes become singletons.
    make(<type-estimate-limited-instance>, singleton: sub-cl)
  else
    // Just note that it's normalized and go on.
    type-estimate-to-be-normalized?(lc) := #f;
    lc
  end
end;

define method type-estimate-normalize(lc :: <type-estimate-limited-collection>)
 => (te :: <type-estimate>)
  // Override method on <type-estimate-limited> for unions in the of: type.
  // bump-normalization-freq(object-class(lc));
  if (~type-estimate-to-be-normalized?(lc))
    // Already been normalized
    lc
  elseif (type-estimate-class(lc) == dylan-value(#"<list>") &
          type-estimate-size(lc) == 0)
    // "Severely limited" collection that canonicalizes to #() only.
    make(<type-estimate-limited-instance>, singleton: #())
  else
    let of   = type-estimate-of(lc);
    // *** Should already be normalized?
    let n-of = when (of) type-estimate-normalize(of) end;
    select (n-of by instance?)
      <type-estimate-union> => type-estimate-CP-expand(
                                 // *** Actually should punt to collection which promotes n-of
                                 //     to something like <object> or <top>, or tighter.
                                 //     OTOH, this can only be megamorphic if lc is already
                                 //     megamorphic.  So punting should never happen here?
                                 curry(type-estimate-base, lc),
                                 curry(make, <type-estimate-limited-collection>,
				       normalize?:, #f,
				       class:, type-estimate-class(lc),
				       concrete-class:, type-estimate-concrete-class(lc),
				       size:, type-estimate-size(lc),
				       dimensions:, type-estimate-dimensions(lc),
				       of:),
                                 n-of);
      singleton(of)         => type-estimate-to-be-normalized?(lc) := #f;
                               lc;
      otherwise             => make(<type-estimate-limited-collection>,
                                    normalize?:     #f,
                                    class:          type-estimate-class(lc),
				    concrete-class: type-estimate-concrete-class(lc),
                                    of:             n-of,
                                    size:           type-estimate-size(lc),
                                    dimensions:     type-estimate-dimensions(lc));
    end
  end
end;

// This function can returns a class type estimate rather than consing
// a new limited function estimate when nothing worthwhile is known,
// hence the result type.

define method type-estimate-normalize(fn :: <type-estimate-limited-function>)
 => (te :: <type-estimate>)
  // CP-expand unions in the args & results.
  // bump-normalization-freq(object-class(fn));
  if (~type-estimate-to-be-normalized?(fn))
    // Already been normalized 
    fn
  else
    // Have to do some work to normalize it.
    let requireds   = type-estimate-requireds(fn);
    let keys        = type-estimate-keys(fn);
    let vals        = type-estimate-values(fn);
    // Normalized pieces
    // *** Should already be normalized?
    let n-requireds :: <type-variable-vector>
      = map(type-estimate-normalize, requireds);
    let n-keys      = when (keys) map(type-estimate-normalize, keys) end;
    let n-vals      = type-estimate-normalize(vals);
    // Check for presence of unions in normalized pieces.
    let unions?     = any?(rcurry(instance?, <type-estimate-union>), n-requireds)
                      | (n-keys & any?(rcurry(instance?, <type-estimate-union>), 
                                       n-keys))
                      | instance?(n-vals, <type-estimate-union>);
    // Check if we want to promote to class <function>, with no extra info.
    if (empty?(n-requireds) & type-estimate-rest?(fn) == #t 
        & n-keys == #f & type-estimate-all-keys?(fn) == #f 
        & type-estimate-match?(n-vals, make(<type-estimate-values>)))
      // Just promote to class <function>, since we really know nothing else.
      make(<type-estimate-class>, class: dylan-value(#"<function>"))
    elseif (every?(\==, requireds, n-requireds) & table=?(keys, n-keys, \==)
          & vals == n-vals & ~unions?)        // No change & no unions.
      type-estimate-to-be-normalized?(fn) := #f;
      fn
    elseif (~unions?)                         // Change, but no unions
      make(<type-estimate-limited-function>, 
           normalize?: #f,
           class:      type-estimate-class(fn),
           requireds:  n-requireds,
           rest?:      type-estimate-rest?(fn),
           keys:       n-keys,
           all-keys?:  type-estimate-all-keys?(fn),
           vals:       n-vals)
    else                                      // CPA expand unions here
      let num-req :: <integer>  = size(n-requireds);
      let num-keys :: <integer> = if (n-keys) size(n-keys) else 0 end;
      let the-keys :: <simple-object-vector> = make(<vector>, size: num-keys);
      let the-vals :: <simple-object-vector> = make(<vector>, size: num-keys);
      when (keys)
	for (a-val keyed-by a-key in n-keys, i :: <integer> from 0)
	  the-keys[i] := a-key;
	  the-vals[i] := a-val;
	end for;
      end when;
      let arg-seq :: <type-variable-vector> = 
        concatenate(n-requireds,            // [0, num-req - 1]
                    case                    // [num-req, num-req + num-keys - 1]
		      n-keys    => as(<type-variable-vector>, the-vals);
		      otherwise => as(<type-variable-vector>, #[]);
                    end,
                    as(<type-variable-vector>, vector(n-vals))); // [num-req + num-keys]
      // format-out("\n*** Normalizing: %s", fn);
      // format-out("\n    Num-req = %d, num-keys = %d", num-req, num-keys);
      // format-out("\n    Arg-seq of size %d = %s", size(arg-seq), arg-seq);
      let result = 
        apply(type-estimate-CP-expand,
              // *** Should really construct limited function which promotes
              //     unions to something like <object>, or maybe <top>.
              curry(type-estimate-base, fn),
              method (#rest args)
                let cpa-reqds
		  = copy-sequence(args, end: num-req);
                let cpa-kvals
		  = copy-sequence(args, start: num-req, end: num-req + num-keys);
                let cpa-vals
		  = args[num-req + num-keys];
                // format-out("\n*** Args:      %s", args);
                // format-out("\n    requireds: %s", cpa-reqds);
                // format-out("\n    keys:      %s", cpa-kvals);
                // format-out("\n    vals:      %s", cpa-vals);
                make(<type-estimate-limited-function>, 
                     normalize?: #f,
                     class:      type-estimate-class(fn),
                     requireds:  cpa-reqds,
                     rest?:      type-estimate-rest?(fn),
                     keys:       when (n-keys)
                                   let new-tbl :: <table> = make(<table>, size: num-keys);
				   for (key in the-keys, new-val in cpa-kvals)
				     new-tbl[key] := new-val
				   end for;
                                   new-tbl
                                 end,
                     all-keys?:  type-estimate-all-keys?(fn),
                     vals:       cpa-vals)
              end, 
              arg-seq);
      // format-out("\n*** Result: %s", result);
      result
    end
  end
end;

define method type-estimate-normalize(cl :: <type-estimate-class>)
 => (te :: <type-estimate-class>)
  // Class type estimate normalization.
  // bump-normalization-freq(object-class(cl));
  if (~type-estimate-to-be-normalized?(cl))
    // Already been normalized
    cl
  elseif (type-estimate-class(cl) == dylan-value(#"<empty-list>"))
    // Normalize <empty-list> to singleton(#()), since sealed & 1 instance.
    make(<type-estimate-limited-instance>, singleton: #())
  else
    // Otherwise normalization is trivial.
    type-estimate-to-be-normalized?(cl) := #f;
    cl
  end
end;

define method type-estimate-normalize(raw :: <type-estimate-raw>)
 => (te :: <type-estimate-raw>)
  // Raw type estimates also normalize trivially.
  // bump-normalization-freq(object-class(raw));
  type-estimate-to-be-normalized?(raw) := #f;
  raw
end;

/// *** Deal with exhaustive partitions, especially in unions.  E.g.,
///     - (limited integers <=0) union (limited integers >=0) = <integer>.
///     - class unions, like all the subclasses of a sealed class:
///       <pair> union <empty-list> = <list>.
///     - some singletons: singleton(#()) = <empty-list>? (Or vice versa?)
define method type-estimate-normalize(un :: <type-estimate-union>)
 => (te :: <type-estimate>)
  // Pull up embedded subunions, collapse subtypes.  O(n^2) in union-size(un).
  // bump-normalization-freq(object-class(un));
  if (~type-estimate-to-be-normalized?(un))
    // Already been normalized
    un
  else
    // Gotta work to normalize it.
    let unionees :: <unionee-sequence> = type-estimate-unionees(un);
    let new-unionees :: <object-deque> = make(<deque>);
    local method canonicalize-unionee (unionee)
            // Embedded unions, supertypes, subtypes.  See DRM pp. 71-72.
            // *** Should already be normalized???
            let norm-unionee = type-estimate-normalize(unionee);
            case
              instance?(norm-unionee, <type-estimate-union>)                  =>
                // Pull up embedded subunion.
                do(canonicalize-unionee, type-estimate-unionees(norm-unionee));
              any?(curry(type-estimate-subtype?, norm-unionee), new-unionees) =>
                // norm-unionee is a subtype of a supertype already in.
                ;
              otherwise                                                       =>
                // Add to the union, removing subtypes already in.
                remove!(new-unionees, norm-unionee, test: type-estimate-subtype?);
                push-last(new-unionees, norm-unionee);
            end
          end;
    // Normalize each unionee, and decide how to add result to canonicals.
    do(canonicalize-unionee, unionees);
    let new-size = size(new-unionees);
    case
      // Union of 0 things, 1 thing, no changes, or changes.
      new-size = 0                          => make(<type-estimate-bottom>);
      new-size = 1                          => new-unionees[0];
      new-size == size(unionees) & // Absence of this test was a subtle bug!
        every?(\==, new-unionees, unionees) => type-estimate-to-be-normalized?(un)
                                                 := #f;
                                               un;
      otherwise                             => make(<type-estimate-union>,
                                                    normalize?: #f,
                                                    unionees:   new-unionees);
    end
  end
end;

define method type-estimate-normalize(val :: <type-estimate-values>)
 => (te :: <type-estimate>)
  // CP-expand unions in the values.
  // bump-normalization-freq(object-class(val));
  let fix  = type-estimate-fixed-values(val);
  let rest = type-estimate-rest-values(val);
  if (~type-estimate-to-be-normalized?(val))
    // Already been normalized
    val
  elseif (any?(rcurry(instance?, <type-estimate-bottom>), fix)
          | instance?(rest, <type-estimate-bottom>))
    // contains a bottom, so it's all bottoms
    make(<type-estimate-values>, 
         rest: make(<type-estimate-bottom>), 
         normalize?: #f)
  else 
    // Gotta work to normalize it
    // Normalize embedded <type-estimate>s.
    // *** Should already be normalized?
    let n-fix   = map(type-estimate-normalize, fix);
    let n-rest  = if (rest) type-estimate-normalize(rest) else #f end;
    // Check for presence of unions in normalized pieces.
    let unions? = any?(rcurry(instance?, <type-estimate-union>), n-fix)
                  | instance?(n-rest, <type-estimate-union>);
    if (every?(\==, n-fix, fix) & n-rest == rest & ~unions?)
      type-estimate-to-be-normalized?(val) := #f;
      val                                   // Neither change nor unions
    elseif (~unions?)                       // Change, but no unions
      make(<type-estimate-values>, normalize?: #f, fixed: n-fix, rest: n-rest)
    else                                    // CPA expand the unions here
      apply(type-estimate-CP-expand, 
            method ()
              // Punt position: new multiple values, but less uniony since we
              // coerce value types to be something nicer.
              make(<type-estimate-values>,
                   fixed: map(method (x)
                                select (x by instance?)
                                  <type-estimate-union> 
                                    // *** Should do better than this.  Include
                                    // <top> types, anyway.
                                    => make(<type-estimate-class>, 
                                            class: dylan-value(#"<object>"));
                                  otherwise
                                    => x;
                                end
                              end,
                              n-fix),
                   rest:  select (n-rest by instance?)
                            singleton(#f)         => #f;
                            // *** Should do better than this.  Should also
                            //     deal with raws, e.g., use <top>?
                            <type-estimate-union> => make(<type-estimate-class>,
                                                          class: dylan-value(
                                                                   #"<object>"));
                            otherwise             => n-rest;
                          end)
            end,
            method (#rest rgs)
              // Construct the values types in the CP-union
              make(<type-estimate-values>, 
                   normalize?: #f, 
                   fixed: copy-sequence(rgs, start: 1),
                   rest: first(rgs))
            end,
            n-rest, n-fix)
    end
  end
end;

///
/// Unions of <type-estimate>s.
///

/*
// *** This is debugging code, normally dead.
define variable *type-estimate-union-args* = #();

// *** This is debugging code, normally dead.
define function show-union-stats (#key data = *type-estimate-union-args*) => ()
  // Summarize the contents of *type-estimate-union-args*.
  format-out("\n\n# of type-estimate-union calls = %d.", size(data));
  let left-subtype  = 0;             // # times left arg is subtype of right
  let right-subtype = 0;             // # times right arg is subtype of left
  let no-subtype    = 0;             // # times no subtype relationship obtains
  let arg-types     = make(<set>);   // Set of arg types which went into unions
  // Loop over data, accumulating summary info.
  for (args in data)
    let arg1      = head(args);
    let arg2      = tail(args);
    add!(arg-types, object-class(arg1));
    add!(arg-types, object-class(arg2));
    // *** Arrgh.  So we can call with-testing-context, so we can do algebra.
    let do-with-testing-context = access(dfmc-testing, do-with-testing-context);
    with-testing-context (#f)
      case
        type-estimate-subtype?(arg1, arg2) => left-subtype  := left-subtype  + 1;
        type-estimate-subtype?(arg2, arg1) => right-subtype := right-subtype + 1;
        otherwise                          => no-subtype    := no-subtype    + 1;
      end
    end
  end;
  // Print out subtypeness freqs
  format-out("\n\nWhen type-estimate-union args are subtypes of each other:");
  format-out("\nLeft subtype of right:  %d\nRight subtype of left: %d\nNo subtype either way:    %d",
             left-subtype, right-subtype, no-subtype);
  // Ok, now we know the arg-types.  Accumulate 2-d table of arg frequencies.
  let arg-type-indices = make(<table>); // Map from arg types to array indices.
  for (arg-type in arg-types, j from 0) // Arbitrarily in <set> iteration order.
    arg-type-indices[arg-type] := j
  end;
  let num-arg-types  = size(arg-types);
  let arg-freq-table = make(<array>, dimensions: list(num-arg-types, num-arg-types), fill: 0);
  let col-totals     = make(<vector>, size: num-arg-types, fill: 0);
  for (args in data)
    let arg1-index = arg-type-indices[object-class(head(args))];
    let arg2-index = arg-type-indices[object-class(tail(args))];
    arg-freq-table[arg1-index, arg2-index] := arg-freq-table[arg1-index, arg2-index] + 1
  end;
  // Print out arg type frequency table.
  format-out("\n\nFrequencies of arguments to type-estimate-union:");
  // Column labels.
  format-out("\n");
  for (col-arg-type in arg-types)
    format-out("\t%s", col-arg-type)
  end;
  format-out("\tRow Totals");
  // Rows.
  for (row-arg-type in arg-types)
    let row-index = arg-type-indices[row-arg-type];
    let row-total = 0;
    format-out("\n%s", row-arg-type);
    for (col-arg-type in arg-types)
      let col-index = arg-type-indices[col-arg-type];
      let count     = arg-freq-table[row-index, col-index];
      format-out("\t%d", count);
      row-total             := row-total + count;
      col-totals[col-index] := col-totals[col-index] + count;
    end;
    format-out("\t%d", row-total)
  end;
  // Last row is for column totals.
  format-out("\nCol Totals");
  for (col-arg-type in arg-types)
    let col-index = arg-type-indices[col-arg-type];
    format-out("\t%d", col-totals[col-index])
  end;
  // return nothing
  values()
end;
*/

define generic type-estimate-union-internal(te1 :: <type-estimate>, te2 :: <type-estimate>)
 => (te :: <type-estimate>);

define method type-estimate-union (te1 :: <type-estimate>, te2 :: <type-estimate>)
 => (te :: <type-estimate>)
  // Record some metering data & trampoline to right place.
  // *type-estimate-union-args* := pair(pair(te1, te2), *type-estimate-union-args*);
  case
    // Metering shows an _awful_ lot of cases have te1 <= te2.  Short-circuit 
    // that early on, rather than consing the type-union and then normalizing it
    // back down!
    // *** 15-Jan-97 metering of Dylan library compilation:
    //   Left subtype of right:  69454 / 74145
    //   Right subtype of left:   4585 / 74145
    //   No subtype either way:    106 / 74145
    //   Left arg is bottom:     67217 / 74145
    instance?(te1, <type-estimate-bottom>) => te2;
    type-estimate-subtype?(te1, te2)       => te2;
    type-estimate-subtype?(te2, te1)       => te1;
    otherwise                              => type-estimate-union-internal(te1, te2);
  end
end;

define macro type-estimate-union-rules-definer
  // Expand a bunch of rules into methods for type-estimate-union.
  { define type-estimate-union-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-union method.
  { ?tname1:name :: ?typ1:name, ?tname2:name :: ?typ2:name <- ?expr:expression }
  => { define method type-estimate-union-internal(?tname1 :: ?typ1, ?tname2 :: ?typ2)
        => (te :: <type-estimate>)
         ?expr
       end }
end;

// *** Doesn't exploit fact that te1 & te2 now known to be incomparable.
define type-estimate-union-rules
  // Just make unions and rely on normalization to do the rest.
  te1 :: <type-estimate>, te2 :: <type-estimate> <- 
    // Default method for non-unions: make a union & normalize.
    make(<type-estimate-union>, unionees: list(te1, te2));
  te :: <type-estimate>, u :: <type-estimate-union> <- 
    // If one arg is a union, add the other to it & normalize.
    make(<type-estimate-union>, unionees: add(type-estimate-unionees(u), te));
  u :: <type-estimate-union>, te :: <type-estimate> <- 
    // If othe arg is a union, add the one to it & normalize.
    make(<type-estimate-union>, unionees: add(type-estimate-unionees(u), te));
  u1 :: <type-estimate-union>, u2 :: <type-estimate-union> <-
    // If both args are unions, concatenate and normalize.
    make(<type-estimate-union>,
         unionees: concatenate(type-estimate-unionees(u1), 
                               type-estimate-unionees(u2)));
end;

///
/// Intersections of <type-estimate>s.
///

define macro type-estimate-intersection-rules-definer
  // Expand a bunch of rules into methods for type-estimate-intersection.
  { define type-estimate-intersection-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-intersection method.
  { ?tname1:name :: ?typ1:name, ?tname2:name :: ?typ2:name <- ?expr:expression }
  => { define method type-estimate-intersection(?tname1 :: ?typ1, ?tname2 :: ?typ2)
        => (te :: <type-estimate>)
         ?expr
       end }
end;

define type-estimate-intersection-rules
  te1 :: <type-estimate>, te2 :: <type-estimate> <- 
    error("*** type-estimate-intersection not implemented yet.")
end;

///
/// Differences of <type-estimate>s.
///

define macro type-estimate-difference-rules-definer
  // Expand a bunch of rules into methods for type-estimate-difference.
  { define type-estimate-difference-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-difference method.
  { ?tname1:name :: ?typ1:name, ?tname2:name :: ?typ2:name <- ?expr:expression }
  => { define method type-estimate-difference(?tname1 :: ?typ1, ?tname2 :: ?typ2)
        => (te :: <type-estimate>)
         ?expr
       end }
end;

define type-estimate-difference-rules
  te1 :: <type-estimate>, te2 :: <type-estimate> <- 
    error("*** type-estimate-difference not implemented yet.")
end;

///
/// Base of <type-estimate>s.  See DRM, p. 48.
///

define method type-estimate-base
    (top :: <type-estimate-top>) => (te :: <type-estimate-top>)
  top
end;

define method type-estimate-base 
    (cl :: <type-estimate-class>) => (te :: <type-estimate-class>)
  // DRM, p. 48: class types are their own base.
  cl
end;

define method type-estimate-base
    (raw :: <type-estimate-raw>) => (te :: <type-estimate-raw>)
  // Raw types are their own base (analogy to classes).
  raw
end;

define method type-estimate-base 
    (lim :: <type-estimate-limited>) => (te :: <type-estimate-class>)
  // DRM, p. 48: generally, the base of a limited type is the class being 
  // limited.  However, this is overridden below for singletons.
  make(<type-estimate-class>, class: type-estimate-class(lim))
end;

define method type-estimate-base 
    (lim :: <type-estimate-limited-collection>) => (te :: <type-estimate-class>)
  make(<type-estimate-class>, 
       class: type-estimate-concrete-class(lim) | type-estimate-class(lim))
end;

define method type-estimate-base (li :: <type-estimate-limited-instance>)
 => (te :: <type-estimate-limited-instance>)
  // DRM, p. 48: Singleton: override the method on general limited types.  The 
  // base of a singleton is the singleton itself.
  li
end;

define method type-estimate-base
    (un :: <type-estimate-union>) => (te :: <type-estimate>)
  // DRM, p. 48: The base of a union is the union of the bases of the unionees.
  let unionees      = type-estimate-unionees(un);
  let base-unionees = map(type-estimate-base, unionees);
  if (every?(\==, base-unionees, unionees) & ~empty?(unionees))
   // Note that the nonempty check is an extension, for empty unions.
   // They'll normalize to bottom, below.
   un
  else
   type-estimate-normalize(make(<type-estimate-union>, unionees: base-unionees))
  end
end;

define method type-estimate-base 
    (mv :: <type-estimate-values>) => (te :: <type-estimate-values>)
  // The base type of a multiple-values type is defined to be a multiple-values
  // of the base types of its components.  This is kind of a guess, since 
  // multiple-values are obviously not a Dylan user type!
  let fixed      = type-estimate-fixed-values(mv);
  let rest       = type-estimate-rest-values(mv);
  let base-fixed = map(type-estimate-base, fixed);
  let base-rest  = if (rest) type-estimate-base(rest) else #f end;
  if (every?(\==, fixed, base-fixed) & rest == base-rest) // No change
    mv
  else                                                    // Some change
    type-estimate-normalize(
      make(<type-estimate-values>, fixed: base-fixed, rest:  base-rest))
  end
end;

define method type-estimate-base 
    (bot :: <type-estimate-bottom>) => (te :: <type-estimate-bottom>)
  // Base type of bottom is bottom.  
  // This is kind of a guess, but the only reasonable one.
  bot
end;

///
/// Structural matching, mostly useful in the test suite.
///

define macro type-estimate-match?-rules-definer
  // Expand rules into method definitions.
  { define type-estimate-match?-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-match? method.
  { ?te1:name, ?te2:name, ?typ:* <- ?expr:expression }
  => { define method type-estimate-match? (?te1 :: ?typ, ?te2 :: ?typ) 
        => (match? :: <boolean>)
         // Insist that the classes be exactly the same, to prevent 
         // spurious matches by inheritance.
         object-class(?te1) == object-class(?te2)
         & ?expr
       end }
typ:
  { ?:expression } => { ?expression }
end;

define type-estimate-match?-rules
  // Could be automagically derived if type classes were macro-defined.
  o1, o2, <object>    <- #f;                                    // Failure case
  // *** Need to be systematic about mapped types here!
  b1, b2, <boolean>   <- b1 == b2;                              // Convenience
  i1, i2, <integer>   <- i1 == i2;                              // Convenience
  c1, c2, <character> <- c1 == c2;                              // Convenience
  s1, s2, <symbol>    <- s1 == s2;                              // Convenience
  o1, o2, <&object>   <- o1 == o2;   // *** do we need something more detailed?
//l1, l2, <list>      <- every?(type-estimate-match?, l1, l2);  // Convenience
  // Native runtime has <vector>s on the loose.
  l1, l2, <sequence>  <- every?(type-estimate-match?, l1, l2);  // Convenience
  t1, t2, <table>     <- table=?(t1, t2, type-estimate-match?); // Convenience

  te1, te2, <type-estimate-top> <- #t;
  te1, te2, <type-estimate-class> <- 
    type-estimate-class(te1) == type-estimate-class(te2);

  te1, te2, <type-estimate-raw> <-
    type-estimate-raw(te1) == type-estimate-raw(te2);

  te1, te2, <type-estimate-values> <- 
    type-estimate-match?(type-estimate-fixed-values(te1), 
                         type-estimate-fixed-values(te2))
    & type-estimate-match?(type-estimate-rest-values(te1), 
                           type-estimate-rest-values(te2));

  te1, te2, <type-estimate-limited-function> <- 
    type-estimate-class(te1) == type-estimate-class(te2)
    & type-estimate-match?(type-estimate-requireds(te1), 
                           type-estimate-requireds(te2))
    & type-estimate-rest?(te1) == type-estimate-rest?(te2)
    & type-estimate-match?(type-estimate-keys(te1), type-estimate-keys(te2))
    & type-estimate-all-keys?(te1) == type-estimate-all-keys?(te2)
    & type-estimate-match?(type-estimate-values(te1), 
                           type-estimate-values(te2));

  te1, te2, <type-estimate-limited-integer> <-
    type-estimate-class(te1) == type-estimate-class(te2)
    & type-estimate-min(te1) = type-estimate-min(te2)
    & type-estimate-max(te1) = type-estimate-max(te2);

  te1, te2, <type-estimate-limited-class> <- 
    type-estimate-subclass(te1) == type-estimate-subclass(te2);

  te1, te2, <type-estimate-limited-instance> <- 
    type-estimate-match?(type-estimate-singleton(te1), 
                         type-estimate-singleton(te2));

  te1, te2, <type-estimate-limited-collection> <- 
    type-estimate-class(te1) == type-estimate-class(te2)
    & type-estimate-match?(type-estimate-of(te1), type-estimate-of(te2))
    & type-estimate-size(te1) = type-estimate-size(te2)
    & type-estimate-dimensions(te1) = type-estimate-dimensions(te2);

  te1, te2, <type-estimate-union> <- 
    type-estimate-match?(type-estimate-unionees(te1), 
                         type-estimate-unionees(te2));

  te1, te2, <type-estimate-bottom> <- #t;
end;

///
/// Instance? of <type-estimate>s.
///

define macro type-estimate-instance?-rules-definer
  // Expand a bunch of rules into methods for type-estimate-instance?.
  { define type-estimate-instance?-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-instance? method.
  { ?oname:name :: ?otyp:name, ?tname:name :: ?ttyp:name <- ?expr:expression }
  => { define method type-estimate-instance?(?oname :: ?otyp, ?tname :: ?ttyp)
        => (instance? :: <boolean>, known? :: <boolean>)
         ?expr
       end }
end;

// *** Deal with second value.
// *** Is use of <model-value> kosher here, or should I be looking at ^mappings?
define type-estimate-instance?-rules
  x :: <model-value>, te :: <type-estimate-top>                <- values(#t, #t);
  x :: <model-value>, te :: <type-estimate-bottom>             <- values(#f, #t);
  x :: <model-value>, te :: <type-estimate-union>              <- // DRM, p. 72.
    any?(curry(type-estimate-instance?, x), type-estimate-unionees(te));
  x :: <model-value>, te :: <type-estimate-values>             <- 
    // Have to recognize the mv-vectors.
    error("*** type-estimate-instance?(_, values) doesn't make sense.");
  x :: <model-value>, te :: <type-estimate-class>              <- 
    values(^instance?(x, type-estimate-class(te)), #t);
  x :: <model-value>, te :: <type-estimate-raw>                <-
    values(^instance?(x, type-estimate-raw(te)), #t);
  x :: <model-value>, te :: <type-estimate-limited-function>   <- 
    values(^instance?(x, type-estimate-class(te)) &
           // Potential bug with singleton(fn) not a subtype of 
           // limited(<function>, ...)?  See comment above 
           // type-estimate-subtype? of limited function.
           type-estimate-subtype?(type-estimate(x), // Relatively crude hammer
                                  te),
           #t);
  x :: <model-value>, te :: <type-estimate-limited-collection> <- // See DRM, p. 125.
    ^instance?(x, dylan-value(#"<limited-collection>"))
      & ^instance?(x, type-estimate-class(te))
      & ^instance?(^element-type(x), as(<&type>, type-estimate-of(te)));
    // ^instance?(x, type-estimate-class(te))
    // & case
    //     type-estimate-size(te)       // Size: keyword given
    //       => value(&size(x)) == type-estimate-size(te)
    //          // *** x not a <stretchy-collection>.
    //          ;
    //     type-estimate-dimensions(te) // Dimensions: keyword given
    //       => // *** dim(x) = type-estimate-dimensions(te)
    //          // *** x not a <stretchy-collection>.
    //          ;
    //     otherwise => #t;             // Neither: no restriction here.
    //   end 
    //& type-estimate-subtype?(***element-type, type-estimate-base(te));
    // error("*** type-estimate-instance?(_, limited-colln) not yet implemented.");
  x :: <model-value>, te :: <type-estimate-limited-instance>   <-
    values(^id?(x, type-estimate-singleton(te)), #t);
  x :: <model-value>, te :: <type-estimate-limited-class>      <-
    values(^instance?(x, type-estimate-class(te))        // Some kind of class
           & ^subtype?(x, type-estimate-subclass(te)),   // which is a subclass
           #t);
  x :: <model-value>, te :: <type-estimate-limited-integer>    <- 
    values(^instance?(x, type-estimate-class(te))         // Appropriate integer
           & (~type-estimate-min(te) | type-estimate-min(te) <= x)
           & (~type-estimate-max(te) | x <= type-estimate-max(te)),
           #t);
end;

///
/// Disjointness of <type-estimate>s.
///

define generic type-estimate-disjoint?-1 (t1 :: <type-estimate>, 
					  t2 :: <type-estimate>)
 => (disjoint? :: <boolean>);

define method type-estimate-disjoint?(t1 :: <type-estimate>, 
                                      t2 :: <type-estimate>)
 => (disjoint? :: <boolean>, unused? :: <boolean>)
  // Only method: 2 types are disjoint if their intersection as sets is empty.
  if (instance?(t1, <type-estimate-bottom>) | 
      instance?(t2, <type-estimate-bottom>))
    // one type is bottom, i.e., empty set, so intersection is always empty.
    values(#t, #t)
  elseif (type-estimate-subtype?(t1, t2) | type-estimate-subtype?(t2, t1))
    // If one is a subtype of the other & either nonempty, then they
    // can't possibly be disjoint.
    values(#f, #t)
  else
    // No subset relation, but could still be some overlap.  See DRM p. 49.
    values(type-estimate-disjoint?-internal(t1, t2), #f)
  end
end;

define inline method type-estimate-unionees(te :: <type-estimate>)
  => (singleton-set :: <unionee-sequence>)
  list(te);
end method;

define function type-estimate-disjoint?-internal
  (t1 :: <type-estimate>, t2 :: <type-estimate>)
     => (disjoint? :: <boolean>)
  if (instance?(t2, <type-estimate-union>))
    // handle this so individual rules can assume t2 is not a union.
    every?(curry(type-estimate-disjoint?, t1), type-estimate-unionees(t2))
  elseif (type-estimate-disjoint?-special-case?(t1))
    type-estimate-disjoint?-1(t1, t2)
  elseif (type-estimate-disjoint?-special-case?(t2))
    type-estimate-disjoint?-1(t2, t1)
  else
    // Last-ditch desperation rule that "just says no."
    #f
  end;
end function;

define method type-estimate-disjoint?-special-case? (t :: <type-estimate>)
 => (res :: singleton(#f))
  #f
end method;

define macro type-estimate-disjoint?-internal-rule-definer
  // Expand a bunch of rules into methods for type-estimate-disjoint?-internal.
  { define type-estimate-disjoint?-internal-rule (?tname1:name :: ?typ1:name)
     end } =>
    { define method type-estimate-disjoint?-special-case? (type :: ?typ1)
       => (res :: singleton(#t))
	#t
      end method;
      define method type-estimate-disjoint?-1
	  (type1 :: ?typ1, type2 :: <type-estimate>) => (disjoint? :: <boolean>)
	"type-estimate-disjoint?-" ## ?typ1 (type1, type2)
      end method }
  { define type-estimate-disjoint?-internal-rule (?tname1:name :: ?typ1:name)
     ?tname2:name :: ?typ2:name <- ?expr:expression ; ?more:*
     end } =>
    { define method "type-estimate-disjoint?-" ## ?typ1
	   (?tname1 :: ?typ1, ?tname2 :: ?typ2)
	 ?expr
      end;
      define type-estimate-disjoint?-internal-rule (?tname1 :: ?typ1)
        ?more
      end }
end macro;

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-raw>)
  // Raw -- disjoint from everything but themselves.  (But since we know
  // by the time we get here that they're not subtypes, always are disjoint!)
  t2 :: <type-estimate>
    <- #t;
  t2 :: <type-estimate-raw>
    <- ~^id?(type-estimate-raw(t1), type-estimate-raw(t2));
end;

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-values>)
  // Multiple values -- disjoint from everything but themselves.
  t2 :: <type-estimate>
    <- #t;
  t2 :: <type-estimate-values>
    <- values-guaranteed-disjoint?(t1, t2);
end;

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-class>)
  // Classes.
  t2 :: <type-estimate>
    <- #t;
  t2 :: <type-estimate-class>
    <- ^classes-guaranteed-disjoint?(type-estimate-class(t1),
                                            type-estimate-class(t2));
end;

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-union>)
  // Unions -- disjoint if components are disjoint.
  // Degenerate case is always #t since every? is #t over the empty set.
  t2 :: <type-estimate>
    <- every?(rcurry(type-estimate-disjoint?, t2), type-estimate-unionees(t1));
end;

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-bottom>)
  // Bottom is always disjoint from everything, including itself.
  t2 :: <type-estimate>        <- #t;
end;

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-limited>)
  // Generic limited types -- (lim, lim) methods on each concrete limited.
  t2 :: <type-estimate>
    // Disjoint if the base type is disjoint.
    <- type-estimate-disjoint?(type-estimate-base(t1), t2);
end;

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-limited-integer>)
  // Limited integers
  t2 :: <type-estimate>
    <- type-estimate-disjoint?(type-estimate-base(t1), t2);
  t2 :: <type-estimate-limited-integer>
    // Compare minima and maxima
    <- limited-integers-guaranteed-disjoint?(t1, t2);
end;

  // *** Limited functions.

  // *** Limited collections (see 3 rules in DRM p. 49).

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-limited-class>)
  // Limited classes (subclasses)
  t2 :: <type-estimate>
    <- type-estimate-disjoint?(type-estimate-base(t1), t2);
  t2 :: <type-estimate-limited-class>
    // Disjoint if no common subclasses (other cases on generic limited, above)
    <- ^classes-guaranteed-disjoint?(type-estimate-subclass(t1),
                                     type-estimate-subclass(t2));
end;  

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-limited-collection>)
  // Limited classes (subclasses)
  t2 :: <type-estimate>
    <- type-estimate-disjoint?(type-estimate-base(t1), t2);
  t2 :: <type-estimate-limited-collection>
    // TODO: extend to handle size and dimensions
    <- ^classes-guaranteed-disjoint?(type-estimate-class(t1),
                                     type-estimate-class(t2))
       | type-estimate-disjoint?(type-estimate-of(t1),
				 type-estimate-of(t2));
end;  

define type-estimate-disjoint?-internal-rule (t1 :: <type-estimate-limited-instance>)
  // Limited instances (singletons): look at the object.  (But since we know
  //  they're not subtypes by the time we get here, they're always disjoint!)
  t2 :: <type-estimate>
    <- ~type-estimate-instance?(type-estimate-singleton(t1), t2);
end;

// *** Think about whether this is the right disjointness relation!
define function values-guaranteed-disjoint? (t1 :: <type-estimate-values>, 
                                             t2 :: <type-estimate-values>)
    => (disjoint? :: <boolean>)
  // Whether these multiple value types are guaranteed disjoint:
  // t1 disj t2 iff NO instance of one can be an instance of the other, ie.,
  // ~ exists x: x :: t1 & x :: t2.
  local method arity (val :: <type-estimate-values>)
            => (fixed :: <integer>, rest? :: <boolean>)
          // Fixed arity (exactly n values):    (n, #f)
          // Infinite arity (n or more values): (n, #t)
          values(size(type-estimate-fixed-values(val)),
                 type-estimate-rest-values(val) ~== #f)
        end,
        method vref (val :: <type-estimate-values>, i :: <integer>)
            => (value :: <type-estimate>, rest? :: <boolean>)
          // Get the ith value type, using #rest value if necessary
          case
            i < size(type-estimate-fixed-values(val)) 
              // Wants a positional value
              => values(type-estimate-fixed-values(val)[i], #f);
            type-estimate-rest-values(val)
              // Out of fixed vals, but can give a rest val
              => values(type-estimate-rest-values(val), #t);
            otherwise
              // No #rest val, so i is out of range for fixed vals.
              => error("%d out of range for multiple values %s", i, val);
          end
        end,
        method disjoint-by-type? () => (disjoint? :: <boolean>)
          // OK, arities overlap: are any pair guaranteed type-disjoint?
          let (nfixed1, nrest1?) = arity(t1);
          let (nfixed2, nrest2?) = arity(t2);
          block (xit)
            for (i from 0)
              when ((i >= nfixed1 & ~nrest1?) | (i >= nfixed2 & ~nrest2?))
                // Ran off the end of one of one w/o #rest, so fail.
                // If either arg has no #rest, this is the failure exit.
                xit(#f, #t)
              end;
              let (t1i, rest1?) = vref(t1, i); // Next value from t1
              let (t2i, rest2?) = vref(t2, i); // Next value from t2
              case
                // Provably disjoint at ith value position?
                type-estimate-disjoint?(t1i, t2i) => xit(#t, #t);
                // Into #rest of both?  No point in looking further.
                rest1? & rest2?                   => xit(#f, #t);
                // Go try some more.
                otherwise                         => ;
              end
            end
          end
        end,
        method disjoint-by-arity? () => (disjoint? :: <boolean>)
          // See if arities could not possibly overlap.  Cheap first test.
          let (fixed1, rest1) = arity(t1);
          let (fixed2, rest2) = arity(t2);
          // Obviously, if both are infinite, they overlap.  3 more cases:
          (~rest1 & ~rest2 & fixed1 ~= fixed2) // Both finite & different
        | (~rest1 &  rest2 & fixed1 <  fixed2) // 1 finite, below low of 2
        | ( rest1 & ~rest2 & fixed1 >  fixed2) // 2 finite, below low of 1
        end;
  values(disjoint-by-arity?() | disjoint-by-type?(), #t)
end;

define function limited-integers-guaranteed-disjoint?
    (t1 :: <type-estimate-limited-integer>, 
     t2 :: <type-estimate-limited-integer>) => (disjoint? :: <boolean>)
  local method above? (min, max)
          case
            ~max | ~min => #f;        // max = +inf or min = -inf
            otherwise   => min > max; // Both finite, so test
          end
        end;
  above?(type-estimate-min(t1), type-estimate-max(t2)) |
  above?(type-estimate-min(t2), type-estimate-max(t1))
end;

define function ^classes-guaranteed-disjoint?(c1 :: <&class>, c2 :: <&class>)
    => (disjoint? :: <boolean>)
  // True if classes c1 & c2 are guaranteed disjoint.  In general, 2 classes
  // are disjoint if they have no common subclasses.  All this squirming around
  // is because that's difficult to determine statically.  See example
  // in guaranteed-joint?.
  local method ^classes-disjoint-by-primary?(c1 :: <&class>, c2 :: <&class>) 
            => (disjoint? :: <boolean>)
          // We can prove c1 & c2 are disjoint if their primary superclasses
          // won't allow diplomatic relations.  This happens when both have
          // primary superclasses, and those primaries aren't themselves
          // in a supertype/subtype relationship.
          // 
          // In fact, you just have to check the leftmost primaries on each:
          // The primaries of each class form a chain, a subset of the CPL.
          // If leftmost-prim-1 is a subclass of leftmost-prim-2, then
          // chain above leftmost-prim-2 is already in 1's CPL, and vice versa.
          let c1-left-primary = ^least-primary-superclass(c1);
          let c2-left-primary = ^least-primary-superclass(c2);
          c1-left-primary ~== #f                       &
          c2-left-primary ~== #f                       &
          ~^subtype?(c1-left-primary, c2-left-primary) &
          ~^subtype?(c2-left-primary, c1-left-primary)
        end,
        method ^classes-disjoint-by-slots?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // DRM p. 57: "... two classes which specify a slot with
          // the same getter or setter generic function are disjoint..."
          // Details cribbed from ^compute-slot-descriptors.
          local method slot-match?
                  (s1 :: <&slot-descriptor>, s2 :: <&slot-descriptor>)
                    => (match? :: <boolean>)
                  // Owners different and either getters or setters match.
                  // (I.e., don't be confused by commonly inherited slots!)
                  ^slot-owner(s1) ~== ^slot-owner(s2) & 
                  (^slot-getter(s1) == ^slot-getter(s2) |
                   (^slot-setter(s1) == ^slot-setter(s2) &
		      ^slot-setter(s1) & ^slot-setter(s2) & #t))
                end;
          let c2-slots = ^slot-descriptors(c2);
          any?(rcurry(member?, c2-slots, test: slot-match?), 
              ^slot-descriptors(c1))
        end,
        method ^classes-disjoint-by-domain?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // *** There is another disjointness test, hence this stub: 
          // disjoint-by-sealed-domains.  This is true if the classes
          // are not known to be joint (i.e., there is no explicitly
          // known common subclass) and there is a sealed domain that
          // guarantees that no new common subclasses can be defined.
          ignore(c1); ignore(c2);
          #f
        end,
        // *** There are a bunch of loose/tight compilation issues:
        //     - how much is known about a class in another library?
        //     - how about if it's not exported from that library?
        //     Ultimately, this is a definition of a library signature.
        // *** Can I exploit model-library(ci) here somehow?
        // *** Memoize this & do recursively, not consing like ^all-subclasses.
        // *** ^direct-subclasses-known-to ? ^worldwide-direct-subclasses ?
        method ^classes-disjoint-by-sealing?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // If you get this far, c1 and c2 _could_ be disjoint, but we need
          // to look at subclasses to be sure.  Ensure no common subclass now,
          // and adequate sealing to guarantee there never will be one.
          local method disjoint-using-subclasses? (c1-subclasses :: <sequence>,
                                                   c2 :: <&class>)
                  // we know all of c1's subclasses, and
                  // we know c2's superclasses (but not necessarily its
                  // subclasses)
                  ~any?(rcurry(^subtype?, c2), c1-subclasses)
                end;
          let c1-subclasses = ^all-subclasses-if-sealed(c1);
          if (c1-subclasses)
            disjoint-using-subclasses?(c1-subclasses, c2)
          else
            let c2-subclasses = ^all-subclasses-if-sealed(c2);
            if (c2-subclasses)
              disjoint-using-subclasses?(c2-subclasses, c1)
            else
              #f
            end
          end
        end,
        method ^classes-guaranteed-disjoint-1?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // First check that one is not a subtype of the other
          ~^subtype?(c1, c2) & ~^subtype?(c2, c1)
          // Now they're known not to be subtypes either way.
          & (  ^classes-disjoint-by-primary?(c1, c2)
             | ^classes-disjoint-by-slots?  (c1, c2)
             | ^classes-disjoint-by-domain? (c1, c2)
             | ^classes-disjoint-by-sealing?(c1, c2))
        end;
  // First look in the cache to see if we already know the answer.
  // *** Investigate <equal-table> now that keys are 2 model-classes?
  let disjoint-cache :: <type-estimate-pair-match-table> 
                    = library-type-estimate-disjoint?-cache(current-library-description());
  let cache-key1    = pair(c1, c2);
  let cache-element = element(disjoint-cache, cache-key1, default: $unfound);
  if (found?(cache-element))
    // Found it in the cache.
    values(cache-element, #t)
  else
    // Have to compute it and remember it.  Index under args both ways.
    let val = ^classes-guaranteed-disjoint-1?(c1, c2);
    disjoint-cache[cache-key1] := (disjoint-cache[pair(c2, c1)] := val);
    val
  end
end;

///
/// Subtypeness of <type-estimate>s.  See DRM pp. 48, 72, 73, and 124.
///
/// These methods were enumerated by considering each of the types in turn,
/// as the left argument.  For each of those, enumerate all the things that
/// could give a #t in the right argument.
///
/// Another way to grok it is to make the 11x11 table for the types of the
/// 2 args, put a check where there's a method, and stare at it.
///
/// *** NB: do these cope with or generate "dont-know" responses correctly?
///

define method type-estimate-subtype?(te1 :: <type-estimate>, 
                                     te2 :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  type-estimate-subtype?-1(te1, te2)
end;

///
/// Top is a supertype of everything
///

define method type-estimate-subtype?(te1 :: <type-estimate>, 
                                     te2 :: <type-estimate-top>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  values(#t, #t)
end;

///
/// Bottom is a subtype of everything, including itself.  Nothing other than 
/// bottom is ever a subtype of bottom.
///

define method type-estimate-subtype?(te1 :: <type-estimate-bottom>, 
                                     te2 :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // Bottom is a subtype of everything.
  values(#t, #t)
end;

// Disambiguating method
define method type-estimate-subtype?(te1 :: <type-estimate-bottom>, 
                                     te2 :: <type-estimate-top>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  values(#t, #t)
end;

///
/// Unions subtype component-wise -- see the 3 rules on DRM p. 72.
///
/// Note that there are other things you can infer if you know the union is an 
/// exhaustive partition, e.g., 
/// type-union(limited(<integer>, min: 0), limited(<integer>, max:0) = <integer>
/// Currently we can't prove this (well, forwards we can, but not backwards).
///

define method type-estimate-subtype?(u :: <type-estimate-union>, 
                                     t :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // DRM p. 72, union subtyping rule 1: u <= t iff every(ui): ui <= t.
  values(every?(rcurry(type-estimate-subtype?, t), type-estimate-unionees(u)),
         #t)
end;

// Disambiguating method
define method type-estimate-subtype?(te1 :: <type-estimate-union>,
                                     te2 :: <type-estimate-top>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  values(#t, #t)
end;


// This can assume that te1 is not <bottom> or <union> or
// <type-estimate-singleton> and te2 is not <top>.
// Its job is to handle te2 = <union>
define generic type-estimate-subtype?-1 (te1 :: <type-estimate>,
					 te2 :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);

define method type-estimate-subtype?-1(te1 :: <type-estimate>, 
				       te2 :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  type-estimate-subtype?-2(te1, te2);
end;


define method type-estimate-subtype?-1(t :: <type-estimate>, 
				       u :: <type-estimate-union>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // DRM p. 72, union subtyping rule 2: t <= u iff exists(ui): t <= ui.
  // If we ever want to do something about smart subtyping between limited
  // types and unions of limited types, can put methods on ...-subtype?-1.
  values(any?(curry(type-estimate-subtype?, t), type-estimate-unionees(u)), #t)
end;

// This can assume that te1 is not <bottom> or <union> or
// <type-estimate-singleton> and te2 is not <top> or <union>
define generic type-estimate-subtype?-2 (te1 :: <type-estimate>,
					 te2 :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);

define method type-estimate-subtype?-2(te1 :: <type-estimate>, 
				       te2 :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // Last-ditch desperation rule: just say no.
  values(#f, #t)
end;


///
/// Multiple values subtype only amongst themselves.
///

define method type-estimate-subtype?-2(v1 :: <type-estimate-values>, 
				       v2 :: <type-estimate-values>)
 => (subtype? :: <boolean>, known? :: <boolean>)
  // Is values type estimate v1 a subtype of v2?
  // * Substitutionality is the key to thinking about this: suppose a
  //   function returned v1 when you expected v2; would that be ok, or not?
  // * Properly speaking, v1 <= v2 iff for every variable x1i taken 
  //   from the ith position of v1 and x2i taken from v2, x1i <= x2i.
  // * Howver, we don't know how many values the receiver will take, so in order
  //   to be correct for ALL POSSIBLE receivers, we have to assume the 
  //   receiver will take INFINITE values.  
  // * Thus everybody gets right-padded with #f's for default values.
  // * Since we don't know how many values are in a #rest value, the type of
  //   anything taken from a rest value is rest U #f (you might be off the end).
  // * If there is no #rest value, then all values off the end are #f.
  let fixed1         = type-estimate-fixed-values(v1);
  let fixed2         = type-estimate-fixed-values(v2);
  let rest1          = type-estimate-rest-values(v1);
  let rest2          = type-estimate-rest-values(v2);
  let defaultf       = make(<type-estimate-limited-instance>, singleton: #f);
  local method type+defaultf (type :: false-or(<type-estimate>))
          => (te :: <type-estimate>)
          // This is the type of anything extracted from a #rest value.  Have 
          // to consider that you might be "off the end," i.e., get #f.
          if (type) 
            type-estimate-union(type, defaultf)
          else
            defaultf
          end
        end;
  let rest1+defaultf = type+defaultf(rest1);
  let rest2+defaultf = type+defaultf(rest2);
  let nfixed1        = size(fixed1);
  let nfixed2        = size(fixed2);
  values(
    block (return)
      if (empty?(fixed1) & instance?(rest1, <type-estimate-bottom>))
        // Special case for values(#rest <bottom>), which is guaranteed to be
        // a subtype of any values spec.  Don't do #f defaulting to #rest value, 
        // since you'll never return from a <bottom>-producer anyway.
        #t
      elseif (~every?(type-estimate-subtype?, fixed1, fixed2))
        // Collection-aligned values not subtypes, so fail.
        #f
      // Now look @ collection-unaligned fixed values & #rest values.
      elseif (nfixed1 > nfixed2) // v1 returns more fixed vals than v2
        // Want (extra fixed1), (rest1 U #f) <= (rest2 U #f)
        for (i from nfixed2 below nfixed1)
          type-estimate-subtype?(fixed1[i], rest2+defaultf) | return(#f)
        finally
          type-estimate-subtype?(rest1+defaultf, rest2+defaultf)
        end
      elseif (nfixed1 < nfixed2) // v1 returns fewer fixed vals than v2
        // Want rest1 U #f <= (extra fixed2), rest2 U #f
        for (i from nfixed1 below nfixed2)
          type-estimate-subtype?(rest1+defaultf, fixed2[i]) | return(#f)
        finally
          type-estimate-subtype?(rest1+defaultf, rest2+defaultf)
        end
      else
        // Identical # of fixed values, which are subtypes.
        // Think about the #rest's. 
        type-estimate-subtype?(rest1+defaultf, rest2+defaultf)
      end
    end,
    #t)
end;

///
/// Raw subtyping.
///

define method type-estimate-subtype?-2(r1 :: <type-estimate-raw>, 
				       r2 :: <type-estimate-raw>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  ^subtype?(type-estimate-raw(r1), type-estimate-raw(r2))
end;

///
/// Class subtyping.
///

define method type-estimate-subtype?-2(t1 :: <type-estimate-class>, 
				       t2 :: <type-estimate-class>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // This generalizes the Dylan subtype? relationship.  If 2 classes are
  // subtype?s of each other, then this is true.  However, this can also
  // be true for other cases with  abstraction, sealing & primariness.
  // For example, c1 can be a subSET of class c2 if c1 is abstract & sealed,
  // and all its subclasses are subtypes of c2.  Then all instances of
  // c1 are indirect instances of some subclass, hence are indirect 
  // instances of c2.
  let c1s-checked = #f;                      // consed lazily below
  let c2          = type-estimate-class(t2);
  local method guaranteed-joint-class?(c1 :: <&class>) => (joint? :: <boolean>)
          // Jointness analysis on the model classes.  These are things
          // which must be true of c1 or, sometimes, all its subclasses.
          // If c1 is a subclass of c2, then they're joint.
          ^subtype?(c1, c2)
          // OK, we know c1 is not a subclass of c2.  How about indirectly?
          | (  // If they're disjoint, then, a fortiori, they're not joint.
               ~^classes-guaranteed-disjoint?(c1, c2)
               // If c1 is open, then we can't GUARANTEE jointness:
               // could make new subs of c1 disjoint from c2.
             & ^class-sealed?(c1)
               // If c1 is concrete, then might have a direct instance, 
               // but c1 isn't a subtype of c2, so direct instance isn't
               // an instance of c2, so not joint.
             & ^class-abstract?(c1)
               // OK, now c1 is abstract & sealed.  Since c1 is abstract, it
               // never has any direct instances.  Since c1 is sealed, can
               // enumerate its direct subclasses at compile time.  So ask
               // direct subclasses of c1: is each subclass joint with c2?
             & guaranteed-joint-class?-recurse(c1))
        end,
        method guaranteed-joint-class?-recurse(c1 :: <&class>) 
            => (joint? :: <boolean>)
          // Recursive case -- memoized for efficiency.  Due to multiple 
          // inheritance, there might be multiple paths to a given subclass.
          // Memoization makes sure we consider each such subclass only 1ce.
          // Split out so table is consed lazly, only when needed.
          unless (c1s-checked ~== #f)
            c1s-checked := make(<table>)
          end;
          let c1-check = element(c1s-checked, c1, default: $unfound);
          if (found?(c1-check))
            // We've been here before, so we already know the answer.
            c1-check
          else
            // Haven't been here before; compute & remember the result.
            // (Other tests above have given #t in order to get here.)
            c1s-checked[c1] := 
              // *** maybe use ^worldwide-direct-subclasses ?
              every?(guaranteed-joint-class?, ^direct-subclasses(c1))
          end
        end;
  values(guaranteed-joint-class?(type-estimate-class(t1)), #t)
end;


///
/// Limited types: limited integers, limited classes, limited functions, 
/// limited collections, and limited instances.
///

define method type-estimate-subtype?-2(lim :: <type-estimate-limited>, 
				       t   :: <type-estimate-class>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // limited(C, ...) <= T if C <= T.  See p. 249.
  // This is a "default" method for limited types in the left argument.
  // Only a singleton can have its base type be itself, and there's a more
  // specific method for that.  So this can never loop.
  values(type-estimate-subtype?(type-estimate-base(lim), t), #t)
end;

define method type-estimate-subtype?-2(te1 :: <type-estimate-class>, 
				       te2 :: <type-estimate-limited>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // Blocking method: if one of the more specific methods on a limited
  // type doesn't grab you, this will say #f.  It blocks the (<class>, <class>)
  // method above, which was giving spurious #t answers on questions like 
  // subtype?(<integer>, <limited-integer>), which is, of course, #f.
  values(#f, #t)
end;

define method type-estimate-subtype?-2(te1 :: <type-estimate-limited>, 
				       te2 :: <type-estimate-limited>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  type-estimate-subtype?-limited(te1, te2)
end;


define method type-estimate-subtype?-limited(te1 :: <type-estimate-limited>,
					     te2 :: <type-estimate-limited>)
  values(#f, #t)
end;

///
/// Limited integers subtype among themselves by interval intersection.
/// Limited integers can (rarely) be subtypes of integer singletons.
/// Limited integers subtype with integer classes by the default limited method
///   above.
///


define method type-estimate-subtype?-limited(li1 :: <type-estimate-limited-integer>, 
					     li2 :: <type-estimate-limited-integer>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // DRM, p. 48: interval analysis.
  let cl1  = type-estimate-class(li1);
  let cl2  = type-estimate-class(li2);
  let min1 = type-estimate-min(li1);
  let min2 = type-estimate-min(li2);
  let max1 = type-estimate-max(li1);
  let max2 = type-estimate-max(li2);
  values(^subtype?(cl1, cl2)                         // Same kind <integer>
       & (min2 == #f | (min1 ~== #f & min1 >= min2))  // No min rstr | above
       & (max2 == #f | (max1 ~== #f & max1 <= max2)), // No max rstr | below
         #t)
end;

// Due to normalization, min ~== max, so this can't happen
/*
define method type-estimate-subtype?-limited(l :: <type-estimate-limited-integer>, 
					     s :: <type-estimate-limited-instance>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // limited integers can be singletons, if interval is exactly 1.
  let l-cl = type-estimate-class(l);
  let s-cl = type-estimate-class(s);
  let sing   = type-estimate-singleton(s);
  let min  = type-estimate-min(l);
  let max  = type-estimate-max(l);
  values(^subtype?(l-cl, s-cl) & min = sing & max = sing, #t)
end;
*/

///
/// Limited classes subtype among themselves by subclassing.
/// Limited classes can (rarely) subtype with class singletons, provided it's
///   sealed and there are no subclasses in the defining library.
/// Limited classes subtype with <class>, by the default limited method above.
///

define method type-estimate-subtype?-limited(lc1 :: <type-estimate-limited-class>,
					     lc2 :: <type-estimate-limited-class>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // subclass(c1) <= subclass(c2) iff c1 <= c2
  values(^subtype?(type-estimate-subclass(lc1), type-estimate-subclass(lc2)),
         #t)
end;

define method type-estimate-subtype?-limited(sub  :: <type-estimate-limited-class>, 
					     sing :: <type-estimate-limited-instance>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // subclass(<foo>) = singleton(<foo>) if <foo> is sealed & no subclasses.
  // (Normalization shouldn't let this happen, but...)
  let subclass = type-estimate-subclass(sub);
  let sing     = type-estimate-singleton(sing);
  values(subclass == sing                         // Same class in both cases
         & ^sealed-with-no-subclasses?(subclass), // and it's sealed and no subclasses here
         #t)
end;

///
/// Limited functions subtype among each other by the usual contravariant rule.
/// They DON'T subtype with singletons of functions, since they constrain only 
///   the signature of the function, not its body.  (*** Do we believe this???)
/// Limited functions subtype with <function> et al. by the default limited 
///   method above.
///

define method type-estimate-subtype?-limited(f1 :: <type-estimate-limited-function>,
					     f2 :: <type-estimate-limited-function>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // Subtyping of limited functions.
  local method fn-code
            (rest?, keys, all?) => (code :: limited(<integer>, min: 0, max: 3))
          // Encode tail behavior for the table below.
          ecase
            ~rest? & ~keys & ~all? => 0; // Expects 0 more args
                      keys & ~all? => 1; // Expects finite # key/val pairs
                              all? => 2; // Expects infinite # key/val pairs
             rest? & ~keys & ~all? => 3; // Expects infinite anything
          end
        end,
        method arg-types>=?(f1, f2) => (contains? :: <boolean>)
          // Think substitutionality: can you use an f1 where f2 is expected?
          let reqs1  = type-estimate-requireds(f1);
          let reqs2  = type-estimate-requireds(f2);
          let rest?1 = type-estimate-rest?(f1);
          let rest?2 = type-estimate-rest?(f2);
          let keys1  = type-estimate-keys(f1);
          let keys2  = type-estimate-keys(f2);
          let all?1  = type-estimate-all-keys?(f1);
          let all?2  = type-estimate-all-keys?(f2);
          case
            (size(reqs1) > size(reqs2)
             | ~every?(type-estimate-subtype?, reqs2, reqs1)
             | (keys1 & keys2 & ~every?(type-estimate-subtype?, keys2, keys1)))
               // Calls of 2 could have too few args for 1 (can't be fixed by
               //   assuming 1 has a rest arg or anything like that).
               // Or types of shared required args don't match.
               // Or both have keywords & types of shared keys don't match.
               // If #rest parameter could be typed (DRM p 83), could hack that.
               => values(#f, #t);
            // If you get here, at least enough args will be given for 1,
            // and that the types match for the requireds which are shared,
            // and that the types match for the keys which are shared.
            (size(reqs1) = size(reqs2))
               // Same number of required args, and types compatible.
               //
               // Each has rest, specifies keys, or all-keys.  From those, we
               // classify the function into one of 4 cases: expects 0 more args,
               // expects finite key/val pairs, expects infinite key/val pairs,
               // and expects infinite anything.  Here's the match test:
               //                 f2
               //           _________________
               //         || 0 | kF | kI | I |
               //    +=======================|
               //    | 0  || Y | N  | N  | N |
               //    |----||---+----+----+---|
               // f1 |kF  || Y | *  | N  | N | * means check keys2 subset keys1
               //    |----||---+----+----+---|
               //    |kI  || Y | Y  | Y  | N |
               //    |----||---+----+----+---|
               //    | I  || Y | Y  | Y  | N |
               //     -----------------------
               => let f1-code = fn-code(rest?1, keys1, all?1);
                  let f2-code = fn-code(rest?2, keys2, all?2);
                  if (f1-code = 1 & f2-code = 1)
                    // Check keys2 are subset of keys1
                    table-key-subset?(keys2, keys1)
                  else
                    // Just have to be in lower triange or diagonal.
                    f1-code >= f2-code
                  end;
             otherwise
               // 1 has fewer required args than 2, so needs #rest or something.
               //
               // Now we have to decide what to do for 2's "extra" args.  
               // 1 must have #rest or some explicit #key words, or #all-keys.
               // * If 1 has #rest only, there can never be too many args.
               //   Since #rest's <object>, there's no type problem.
               // * But if 1 has either explicit #key words or #all-keys,
               //   it expects key/val pairs.  2 is giving general args though,
               //   in its remaining required args.  So no match.
               => values(rest?1 & ~keys1 & ~all?1, #t);
          end
        end;
  // The way to think about this is substitutionality: can we use f1 where we
  // were expecting f2?  In order for that to happen, 2 things must be true:
  //
  // [1] values(f1) <= values(f2), i.e., we have to be able to accept the values
  //     returned by f1.
  //
  // [2] args(f1) >= args(f2), i.e., the args of f1 must be AT LEAST AS GENERAL
  //     as those for f2, so it can be called where f2 was expected.  
  //
  // This means args type THE OTHER WAY from values; hence the phrase 
  // "contravariant function typing."
  values(arg-types>=?(f1, f2)
         & type-estimate-subtype?(type-estimate-values(f1), type-estimate-values(f2)),
         #t)
end;

///
/// Limited collections subtype among themselves.
/// Limited collections can (rarely) subtype with singletons, e.g., 
///  singleton(#()).
/// Limited collections subtype <collection> et al. by the default limited 
///  method above.
///

define method type-estimate-subtype?-limited(lc1 :: <type-estimate-limited-collection>, 
					     lc2 :: <type-estimate-limited-collection>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // Limited collections: DRM, pp. 124-125.  (The case where lc1 is a singleton
  // is taken care of in the singleton method above.)
  let class1 = type-estimate-class(lc1);
  let class2 = type-estimate-class(lc2);
  let of1    = type-estimate-of(lc1);
  let of2    = type-estimate-of(lc2);
  let size1  = type-estimate-size(lc1);
  let size2  = type-estimate-size(lc2);
  let dim1   = type-estimate-dimensions(lc1);
  let dim2   = type-estimate-dimensions(lc2);
  values(if (~dim1 & ~dim2)
           // Rule 1: nobody specifies dimensions:.
           ^subtype?(class1, class2)
           & (~size2 | size1 = size2)  // *** why not size1 >= size2 ?
           & ((of1 == #f & of2 == #f) |    // Both #f
              (of1 ~== #f & of2 ~== #f &   // Or both not #f & equivalent.
               type-estimate=?(of1, of2))) // *** why not type-estimate-subtype?(of1, of2)?
         else
           // Rule 2: somebody specified dimensions:.
           ^subtype?(class1, class2)
           & dim1                                  // *** why not size1 & dim2 = rank1?
           & (~dim2 | every?(\=, dim1, dim2))      // *** why not >=?
           & (size2 | reduce(\*, 1, dim1) = size2) // *** why not >=?
           & ((of1 == #f & of2 == #f) |            // Both #f
              (of1 ~== #f & of2 ~== #f &           // Or both not #f & equivalent.
               type-estimate=?(of1, of2)))         // *** why not type-estimate-subtype?(of1, of2)?
         end, 
         #t)
end;

define method type-estimate-subtype?-limited(lc :: <type-estimate-limited-collection>, 
					     s  :: <type-estimate-limited-instance>)
 => (subtype? :: <boolean>, known? :: <boolean>);
 // Users can't say limited(<list>, size: 0), but we might be able to infer it.
 values(type-estimate-class(lc) == dylan-value(#"<list>")
        & type-estimate-size(lc) == #()
	& type-estimate-singleton(s) == #(),
        #t)
end;

///
/// Singletons subtype with anything else if they're an instance of it.
///

define method type-estimate-subtype?(s :: <type-estimate-limited-instance>, 
				     t :: <type-estimate>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // DRM, p. 48: "by object type and identity"
  type-estimate-instance?(type-estimate-singleton(s), t)
end;

// Disambiguating method
define method type-estimate-subtype?(s :: <type-estimate-limited-instance>, 
				     t :: <type-estimate-top>)
 => (subtype? :: <boolean>, known? :: <boolean>);
  // DRM, p. 48: "by object type and identity"
  values(#t, #t)
end;


///
/// Retraction of type-estimates.
///

define method type-estimate-retract (ref :: <dfm-ref>)
 => (did-anything? :: <boolean>);
  let cache = library-type-cache(current-library-description());
  // The "right" way to do this is to remove ref from the cache, and then
  // for each (recursive) dependent of ref's type, take the difference with
  // ref's type, and so on.
  //
  // Ok, that's hard, since don't yet have a theory of type-estimate-difference!
  // So just yank ref out of the cache, and yank out all dependents, as well.
  // Compares like a baseball bat to a scalpel, but works.
  local method type-estimate-retract-1 (ref :: <dfm-ref>) => (da? :: <boolean>)
          // Simple-minded recursive key removal from the cache.  Return #t
          // if we did anything, else #f.
          let type-var = cached-type-variable(ref, cache);
          type-var & 
          begin 
            // It was in the cache, so we have work to do.
            retract-cached-type-variable(ref, cache);
            do(method (just) type-estimate-retract-1(justification-lhs(just)) end,
               type-variable-supportees(type-var));
            #t
          end
        end;
  type-estimate-retract-1(ref)
end;
