Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic guaranteed-joint?(t1 :: <&type>, t2 :: <&type>)
   => (res :: <boolean>);


define method guaranteed-joint?(t1 :: <&type>, t2 :: <&type>) 
    => (result :: <boolean>)
  ^subtype?(t1, t2)
end;


define method guaranteed-joint?(c1 :: <&class>, c2 :: <&class>)
    => (result :: <boolean>)
  let results :: <list> = #();
  local method guaranteed-joint-class? (c1 :: <&class>)
          ^subtype?(c1, c2)
          | (  ~^classes-guaranteed-disjoint?(c1, c2)
             & ^class-sealed?(c1)
             & ^class-abstract?(c1)
             & guaranteed-joint-class?-recurse(c1))
        end,
        method guaranteed-joint-class?-recurse(c :: <&class>)
          let item :: false-or(<pair>) = assoc(c, results);
          if (item)
            item.tail;
          else
            let result = every?(guaranteed-joint-class?, ^direct-subclasses(c));
            results := pair(pair(c, result), results);
            result;
          end;
        end,
        method assoc(k, l :: <list>) 
          if (empty?(l))
            #f
          else
            let item :: <pair> = l.head;
            if (item.head == k)
              item;
            else
              assoc(k, l.tail);
            end;
          end;
        end;
  guaranteed-joint-class?(c1);
end;

define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&type>)
    => (result :: <boolean>)
  // Singleton model types can be quickly decided here.
  ^instance?(^singleton-object(t1), t2)
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&singleton>)
    => (result :: <boolean>)
  // Singleton model types can be quickly decided here.
  let obj = ^singleton-object(t2);
  if (instance?(obj, <&class>))
    ^subtype?(t1, t2);
  else
    ^instance?(t1, t2)
  end;
end;

define method guaranteed-joint?(u :: <&union>, t2 :: <&type>)
    => (result :: <boolean>)
  every?(rcurry(guaranteed-joint?, t2), u.^union-members);
end;

define method guaranteed-joint?(t :: <&type>, u :: <&union>)
    => (result :: <boolean>)
  any?(curry(guaranteed-joint?, t), u.^union-members);
end;

define method guaranteed-joint?(u1 :: <&union>,  u2 :: <&union>)
    => (result :: <boolean>)
  every?(curry(guaranteed-joint?, u2), u1.^union-members);
end;

define method guaranteed-joint?(t1 :: <&limited-type>, t2 :: <&type>)
    => (result :: <boolean>)
  guaranteed-joint?(^base-type(t1), t2);
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&limited-type>)
    => (result :: <boolean>)
  guaranteed-joint?(t1, ^base-type(t2));
end;

define method guaranteed-joint?(t1 :: <&limited-type>, t2 :: <&limited-type>)
    => (result :: <boolean>)
  guaranteed-joint?(^base-type(t1), ^base-type(t2));
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&subclass>)
    => (result :: <boolean>)
  #f  // Could refine this?
end;

define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&subclass>)
    => (result :: <boolean>)
  let obj = ^singleton-object(t1);
  if (instance?(obj, <&class>))
    ^subtype?(obj, t2.^subclass-class);
  else
    #f
  end;
end;

define method guaranteed-joint?(t1 :: <&subclass>, t2 :: <&subclass>)
    => (result :: <boolean>)
  ^subtype?(^subclass-class(t1), ^subclass-class(t2));
end;

define method guaranteed-joint?(t1 :: <values-type>, t2 :: <&type>)
    => (result :: <boolean>)
  #f
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <values-type>)
    => (result :: <boolean>)
  #f
end;

define method guaranteed-joint?(t1 :: <values-type>, t2 :: <values-type>)
    => (result :: <boolean>)
  break();
  ^subtype?(t1, t2);
end;


// Disjointness

define generic guaranteed-disjoint?(t1 :: <&type>, t2 :: <&type>)
  => (res :: <boolean>);

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&type>)
    => (result :: <boolean>)
  // Coerce both args to a <type-estimate>.
  if (guaranteed-joint?(t1, t2) | guaranteed-joint?(t2, t1))
    #f;
  else
    guaranteed-disjoint?-internal(t1, t2);
  end;
end;

define method guaranteed-disjoint?(t1 :: <&singleton>, t2 :: <&type>)
    => (result :: <boolean>)
  // Singleton model types can be quickly decided here.
  ~^instance?(^singleton-object(t1), t2)
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&singleton>)
    => (result :: <boolean>)
  // Singleton model types can be quickly decided here.
  ~^instance?(^singleton-object(t2), t1)
end;

define method guaranteed-disjoint?(c1 :: <&class>, c2 :: <&class>)
    => (result :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(c1, c2)
end;

// I just happen to know that <top> will only occur on the right
define method guaranteed-disjoint?(c1 :: <&type>, 
                                   c2 :: <&top-type>)
    => (result :: <boolean>)
  #f
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&bottom-type>)
    => (result :: <boolean>)
  #t
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&raw-type>)
    => (result :: <boolean>)
  #t
end;

define method guaranteed-disjoint?(t1 :: <&raw-type>, t2 :: <&type>)
    => (result :: <boolean>)
  #t
end;

define method guaranteed-disjoint?(t1 :: <&raw-type>, t2 :: <&raw-type>)
    => (result :: <boolean>)
//  t1 ~== t2
//   There seems to be some dispute over whether raw types have a hierarchy.
//   Assume they have for now.
  ~(^subtype?(t1, t2) | ^subtype?(t2, t1))
end;

define method guaranteed-disjoint?(t1 :: <values-type>, t2 :: <&type>)
    => (result :: <boolean>)
  select (t1.fixed-types.size)
    0 => t1.values-rest-type & guaranteed-disjoint?(t1.values-rest-type, t2);
    1 => guaranteed-disjoint?(t1.fixed-types[0], t2);
    otherwise => #t;
  end
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <values-type>)
    => (result :: <boolean>)
  guaranteed-disjoint?(t2, t1)
end;

define method guaranteed-disjoint?(t1 :: <values-type>, t2 :: <values-type>)
    => (result :: <boolean>)
  values-guaranteed-disjoint?(t1, t2);
end;



define macro guaranteed-disjoint?-internal-rules-definer
  // Expand a bunch of rules into methods for guarantee-disjoint?-internal.
  { define guaranteed-disjoint?-internal-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a guaranteed-disjoint?-internal method.
  { ?tname1:name :: ?typ1:name, ?tname2:name :: ?typ2:name <- ?expr:expression }
  => { define method guaranteed-disjoint?-internal(?tname1 :: ?typ1, 
                                                      ?tname2 :: ?typ2)
           => (result :: <boolean>)
         ?expr
       end }
end;

// *** Method ambiguity conflicts, e.g., (<union>, <class>)?


define guaranteed-disjoint?-internal-rules
  // Last-ditch desperation rule that "just says no."
  t1 :: <&type>, t2 :: <&type> <- #f ;

  // Raw -- disjoint from everything but themselves.  (But since we know
  // by the time we get here that they're not subtypes, always are disjoint!)
  t1 :: <&type>, t2 :: <&raw-type>
    <- #t;
  t1 :: <&raw-type>, t2 :: <&type>
    <- #t;
  t1 :: <&raw-type>, t2 :: <&raw-type>
    <- #t;

  // Multiple values -- disjoint from everything but themselves.
  t1 :: <values-type>, t2 :: <&type>
    <- #t;
  t1 :: <&type>, t2 :: <values-type>
    <- #t;
  t1 :: <values-type>, t2 :: <values-type>
    <- values-guaranteed-disjoint?(t1, t2);

  // Classes.
  t1 :: <&class>, t2 :: <&type>
    <- #t;
  t1 :: <&type>, t2 :: <&class>
    <- #t;
  t1 :: <&class>, t2 :: <&class>
    // *** Perhaps should use 2nd value of #f at sealing boundaries?
    <- ^classes-guaranteed-disjoint?(t1, t2);

  // Unions -- disjoint if components are disjoint.
  t1 :: <&union>, t2 :: <&type>
    <- every?(rcurry(guaranteed-disjoint?, t2), t1.^union-members);

  t1 :: <&union>, t2 :: <&class>
    <- every?(rcurry(guaranteed-disjoint?, t2), t1.^union-members);

  t1 :: <&type>, t2 :: <&union>
    <- every?(rcurry(guaranteed-disjoint?, t1), t2.^union-members);

  t1 :: <&class>, t2 :: <&union>
    <- every?(rcurry(guaranteed-disjoint?, t1), t2.^union-members);

  t1 :: <&union>, t2 :: <&union>
    <- every?(rcurry(guaranteed-disjoint?, t2), t1.^union-members);

/*
  I don`t know about this!
  // Bottom is always disjoint from everything, including itself.
  // Degenerate case is always #t since every? is #t over the empty set.
  t1 :: <&bottom-type>, t2 :: <&type>        <- #t;
  t1 :: <&type>, t2 :: <&bottom-type>        <- #t;
  t1 :: <&bottom-type>, t2 :: <&bottom-type> <- #t;
*/
  // Generic limited types -- (lim, lim) methods on each concrete limited.
  t1 :: <&limited-type>, t2 :: <&type>
    // Disjoint if the base type is disjoint.
    <- guaranteed-disjoint?(^base-type(t1), t2);
  t1 :: <&type>, t2 :: <&limited-type>
    <- guaranteed-disjoint?(^base-type(t2), t1);

  // Limited integers
  t1 :: <&limited-integer>, t2 :: <&limited-integer>
    // Comare minima and maxima
    <- limited-integers-guaranteed-disjoint?(t1, t2);

  // *** Limited functions.

  // *** Limited collections (see 3 rules in DRM p. 49).

  // Limited classes (subclasses) get caught here
  t1 :: <&singleton>, t2 :: <&singleton>
    <- singletons-guaranteed-disjoint?
        (t1, t1.^singleton-object, t2, t2.^singleton-object);
  
  // Limited instances (singletons): look at the object.  (But since we know
  //  they're not subtypes by the time we get here, they're always disjoint!)
  t1 :: <&singleton>, t2 :: <&type>
    <- ~^instance?(t1.^singleton-object, t2);
  t1 :: <&type>, t2 :: <&singleton>
    <- ~^instance?(t2.^singleton-object, t1);
end;

define method singletons-guaranteed-disjoint?
  (t1, obj1, t2, obj2)
  => (result :: <boolean>);
  ~^instance?(obj1, t2);
end;

define method singletons-guaranteed-disjoint?
  (t1, obj1 :: <&class>, t2, obj2 :: <&class>)
  => (result :: <boolean>);
  ^classes-guaranteed-disjoint?(obj1, obj2);
end;

define function values-guaranteed-disjoint? (t1 :: <values-type>, 
                                             t2 :: <values-type>)
    => (result :: <boolean>)
  // Whether these multiple value types are guaranteed disjoint:
  // t1 disj t2 iff NO instance of one can be an instance of the other, ie.,
  // ~ exists x: x :: t1 & x :: t2.
  local method arity (val :: <values-type>)
            => (fixed :: <integer>, rest? :: <boolean>)
          // Fixed arity (exactly n values):    (n, #f)
          // Infinite arity (n or more values): (n, #t)
          values(val.fixed-types.size, ~(~val.values-rest-type));
        end,
        method vref (val :: <values-type>, i :: <integer>)
            => (type :: <&type>, rest? :: <boolean>)
          // Get the ith value type, using #rest value if necessary
          case
            i < val.fixed-types.size
              // Wants a positional value
              => values(val.fixed-types[i], #f);
            val.values-rest-type
              // Out of fixed vals, but can give a rest val
              => values(val.values-rest-type, #t);
            otherwise
              // No #rest val, so i is out of range for fixed vals.
              => error("%d out of range for multiple values %s", i, val);
          end
        end,
        method disjoint-by-type? (fixed1, rest1?, fixed2, rest2?) 
          => (result :: <boolean>)
          // OK, arities overlap: are any pair guaranteed type-disjoint?
          block (xit)
            for (i from 0)
              when ((i >= fixed1 & ~rest1?) | (i >= fixed2 & ~rest2?))
                // Ran off the end of one of one w/o #rest, so fail.
                // If either arg has no #rest, this is the failure exit.
                xit(#f);
              end;
              let (t1i, rest1?) = vref(t1, i); // Next value from t1
              let (t2i, rest2?) = vref(t2, i); // Next value from t2
              case
                // Provably disjoint at ith value position?
                guaranteed-disjoint?(t1i, t2i) => xit(#t);
                // Into #rest of both?  No point in looking further.
                rest1? & rest2?                   => xit(#f);
                // Go try some more.
                otherwise                         => ;
              end
            end
          end
        end,
        method disjoint-by-arity? (fixed1, rest1, fixed2, rest2) 
          => (result :: <boolean>)
          // See if arities could not possibly overlap.  Cheap first test.
          // Obviously, if both are infinite, they overlap.  3 more cases:
          (~rest1 & ~rest2 & fixed1 ~= fixed2) // Both finite & different
        | (~rest1 &  rest2 & fixed1 <  fixed2) // 1 finite, below low of 2
        | ( rest1 & ~rest2 & fixed1 >  fixed2) // 2 finite, below low of 1
        end;
  let (fixed1, rest1) = arity(t1);
  let (fixed2, rest2) = arity(t2);

  disjoint-by-arity?(fixed1, rest1, fixed2, rest2) 
  | disjoint-by-type?(fixed1, rest1, fixed2, rest2) 
end;

define function limited-integers-guaranteed-disjoint?
    (t1 :: <&limited-integer>, 
     t2 :: <&limited-integer>) => (b :: <boolean>)
  local method above? (min, max)
          case
            ~max | ~min => #f;        // max = +inf or min = -inf
            otherwise   => min > max; // Both finite, so test
          end
        end;
  above?(^limited-integer-min(t1), ^limited-integer-max(t2)) |
  above?(^limited-integer-min(t2), ^limited-integer-max(t1))
end;

define inline function type-disjoint?-cache()
  let lib = current-library-description();
  library-type-estimate-disjoint?-cache(lib)
  | (library-type-estimate-disjoint?-cache(lib)
       := make(<type-estimate-pair-match-table>))
end;

define function ^classes-guaranteed-disjoint?(c1 :: <&class>, c2 :: <&class>)
    => (b :: <boolean>)
  // True if classes c1 & c2 are guaranteed disjoint.  In general, 2 classes
  // are disjoint if they have no common subclasses.  All this squirming around
  // is because that's difficult to determine statically.  See example
  // in guaranteed-joint?.
  local method ^classes-disjoint-by-primary?(c1 :: <&class>, c2 :: <&class>) 
            => (result :: <boolean>)
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
          let ans
            = c1-left-primary ~== #f                       &
              c2-left-primary ~== #f                       &
              ~^subtype?(c1-left-primary, c2-left-primary) &
              ~^subtype?(c2-left-primary, c1-left-primary);
          gts-debug("jointedness", "^classes-disjoint-by-primary? %=,%= : %=.\n",
            c1, c2, ans);
          ans
        end,
        method ^classes-disjoint-by-slots?(c1 :: <&class>, c2 :: <&class>)
            => (b :: <boolean>)
          // DRM p. 57: "... two classes which specify a slot with
          // the same getter or setter generic function are disjoint..."
          // Details cribbed from ^compute-slot-descriptors.
          local method slot-match?
                  (s1 :: <&slot-descriptor>, s2 :: <&slot-descriptor>)
                    => (result :: <boolean>)
                  // Owners different and either getters or setters match.
                  // (I.e., don't be confused by commonly inherited slots!)
                  ^slot-owner(s1) ~== ^slot-owner(s2) & 
                  (^slot-getter(s1) == ^slot-getter(s2) |
                   ^slot-setter(s1) == ^slot-setter(s2))
                end;
          let c2-slots = ^slot-descriptors(c2);
          let ans 
            = any?(rcurry(member?, c2-slots, test: slot-match?), 
                  ^slot-descriptors(c1));
          gts-debug("jointedness", "^classes-disjoint-by-slots? %=,%= : %=.\n",
            c1, c2, ans);
          ans
        end,
        method ^classes-disjoint-by-domain?(c1 :: <&class>, c2 :: <&class>)
            => (result :: <boolean>)
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
            => (b :: <boolean>)
          // If you get this far, c1 and c2 _could_ be disjoint, but we need
          // to look at subclasses to be sure.  Ensure no common subclass now,
          // and adequate sealing to guarantee there never will be one.
          local method disjoint-using-subclasses? (c1-subclasses :: <sequence>,
                                                   c2 :: <&class>)
                  // we know all of c1's subclasses, and
                  // we know c2's superclasses (but not necessarily its
                  // subclasses)
                  ~any?(rcurry(^subtype?, c2), c1-subclasses);
                end;
          let c1-subclasses = ^all-subclasses-if-sealed(c1);
          let ans
            = if (c1-subclasses)
                disjoint-using-subclasses?(c1-subclasses, c2);
              else
                let c2-subclasses = ^all-subclasses-if-sealed(c2);
                if (c2-subclasses)
                  disjoint-using-subclasses?(c2-subclasses, c1);
                else
                  #f;
                end;
              end;
          gts-debug("jointedness", "^classes-disjoint-by-sealing? %=,%= : %=.\n",
            c1, c2, ans);
          ans
        end,
        method ^classes-guaranteed-disjoint-1?(c1 :: <&class>, c2 :: <&class>)
            => (b :: <boolean>)
          let ans = 
            // First check that one is not a subtype of the other
            ~^subtype?(c1, c2) & ~^subtype?(c2, c1)
            // Now they're known not to be subtypes either way.
            & (  ^classes-disjoint-by-primary?(c1, c2)
               | ^classes-disjoint-by-slots?  (c1, c2)
               | ^classes-disjoint-by-domain? (c1, c2)
               | ^classes-disjoint-by-sealing?(c1, c2));
          gts-debug("jointedness", "^classes-guaranteed-disjoint-1? %=,%= : %=.\n",
                c1, c2, ans);
          ans;
        end;
  // First look in the cache to see if we already know the answer.
  // *** Investigate <equal-table> now that keys are 2 model-classes?
  let disjoint-cache :: <type-estimate-pair-match-table> 
                    = type-disjoint?-cache();
  let cache-key1    = pair(c1, c2);
  let cache-element = element(disjoint-cache, cache-key1, default: not-found());
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



define function potentially-joint? 
    (type-estimate :: <&type>, type :: <&type>) => (res :: <boolean>)
  // Not provably DISjoint.
  ~guaranteed-disjoint?(type-estimate, type)
end;

// Disjointness cache
define class <pair-type-key-table> (<table>)
end class;

define function pair-type-key-test 
  (k1 :: <pair>, k2 :: <pair>)
  let head1 = k1.head;
  let tail1 = k1.tail;
  let head2 = k2.head;
  let tail2 = k2.tail;

  ((head1 == head2) & (tail1 == tail2))
  | (^type-equivalent?(head1, head2) & ^type-equivalent?(tail1, tail2))
end;


define method table-protocol (t :: <pair-type-key-table>)
    => (test :: <function>, hash :: <function>)
  values(pair-type-key-test, pair-type-key-hash);
end method;

define constant <type-estimate-pair-match-table> = <pair-type-key-table>;

define method guaranteed-preceding-specializer? (type1  :: <&type>, 
                                                 type2  :: <&type>, 
                                                 arg-te :: <&type>)
  ^subtype?(type1, type2)
end;
