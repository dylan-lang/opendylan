Module:   dfmc-optimization
Synopsis: Compile-time method selection
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: It's probably the right thing to do the detailed compatibility 
// checking even in loose mode and it's not going to be exploited. Take
// care that the checking doesn't get switched off by the loose mode
// switch.

//// Debug switches.

define thread variable *trace-dispatch*            = #f;
define thread variable *trace-dispatch-failure*    = #f;
// define thread variable *trace-selection-success*   = #f;
define thread variable *trace-selection-failure*   = #f;
define thread variable *trace-call-cache-success*  = #f;
define thread variable *trace-call-cache-failure*  = #f;
define thread variable *colorize-dispatch*         = #t;
define thread variable *warn-about-bogus-upgrades* = #f;
define thread variable *colorize-bogus-upgrades*   = #t;

define constant <method-sequence> = <list>;

define macro format-when
  { format-when(?test:expression, ?args:*) }
    => { if (?test) format-out(?args) end }
end macro;

define macro note-when
  { note-when(?test:expression, ?args:*) }
    => { if (?test) note(?args) end }
end macro;

//// Typist interface.

define constant <type-or-est> = type-union(<&type>, <type-estimate>);

///
/// guaranteed-joint? (<type-or-est>, <type-or-est>) => (joint? :: <boolean>)
///
/// Returns true only if all instances conforming to the type-estimate
/// are guaranteed to be instances of the given Dylan type.
///
/// Test case: 
///
/// define sealed abstract class <a> (<object>) end; 
/// define open   concrete class <b> (<object>) end;
/// define        concrete class <c> (<a>, <b>) end;
///
/// guaranteed-joint?(lookup-value(#"<a>"), lookup-value(#"<b>")); should
///   return #t.  Note <b> is trying especially hard to be uncooperative.
///   <c> can be open or sealed, as you wish.
///
///  Given a generic function and type estimates for its required
///  arguments, guaranteed-joint? is a conservative test used to determine
///  whether the arguments will always lie within a sealed domain. If they
///  do (or if the generic function itself is completely sealed) the set of
///  potentially applicable methods can be determined using
///  potentially-joint?. If there's just one potentially applicable method,
///  and that method is always applicable (guaranteed-joint? again), we can
///  do a simple compile-time dispatch to that lone method.
///

define generic guaranteed-joint?(t1 :: <type-or-est>, t2 :: <type-or-est>)
   => (res :: <boolean>);

define method guaranteed-joint?(t1 :: <&type>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Coerce first arg to a <type-estimate>.
  type-estimate-subtype?(as(<type-estimate>, t1), t2)
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce second arg to a <type-estimate>.
  type-estimate-subtype?(t1, as(<type-estimate>, t2))
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce both args to <type-estimate>s.
  type-estimate-subtype?(as(<type-estimate>, t1), as(<type-estimate>, t2))
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Both are <type-estimate>s, so hand off.
  type-estimate-subtype?(t1, t2)
end;

define method guaranteed-joint?(t1 :: <type-estimate-limited-instance>,
				t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton type estimates can be decided quickly, w/o typist overhead. 
  ^instance?(type-estimate-singleton(t1), t2)
end;
   
// Disambiguating methods
define method guaranteed-joint?(t1 :: <type-estimate-limited-instance>,
				t2 :: <&top-type>)
 => (res :: singleton(#t))
  #t
end;
define method guaranteed-joint?(t1 :: <type-estimate-limited-instance>,
				t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton model types can be decided quickly, w/o typist overhead. 
  ^instance?(^singleton-object(t1), t2)
end;

// Disambiguating methods
define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&top-type>)
 => (res :: singleton(#t))
  #t
end;
define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <&top-type>)
 => (res :: singleton(#t))
  // Top types can be quickly decided, w/o typist overhead.
  #t
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&top-type>)
 => (res :: singleton(#t))
  // Top types can be quickly decided, w/o typist overhead.
  #t
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  // Bottom can be quickly decided, w/o typist overhead.
  #f
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  // Bottom can be quickly decided, w/o typist overhead.
  #f
end;

define method guaranteed-joint? (t1 :: <type-estimate-bottom>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;
  
define method guaranteed-joint? (t1 :: <&bottom-type>, t2 :: <type-estimate-bottom>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;

define method guaranteed-joint? (t1 :: <type-estimate-bottom>, t2 :: <type-estimate-bottom>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;

define method guaranteed-joint? (t1 :: <&bottom-type>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;

///
/// guaranteed-disjoint?(t1, t2) -- #t if any instance of t1 is guaranteed 
///    never to be an instance of t2.  NB: If t1 is a subtype of t2, then EVERY
///    instance of t1 is an instance of t2.  If t2 is a subtype of t1, then SOME
///    (indirect) instances of t1 are (direct) instances of t2.  So t1 & t2
///    must be incomparable in order to be disjoint.
///
///    Disjointness is a symmetric, non-transitive, anti-reflexive relation.
///
///    DRM p.49 gives disjointness rules.
///

define generic guaranteed-disjoint?(t1 :: <type-or-est>, t2 :: <type-or-est>)
  => (res :: <boolean>);

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Coerce first arg to a <type-estimate>.
  type-estimate-disjoint?(as(<type-estimate>, t1), t2)
end;

define method guaranteed-disjoint?(t1 :: <type-estimate>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce second arg to a <type-estimate>.
  type-estimate-disjoint?(t1, as(<type-estimate>, t2))
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce both args to a <type-estimate>.
  type-estimate-disjoint?(as(<type-estimate>, t1), as(<type-estimate>, t2))
end;

define method guaranteed-disjoint?(t1 :: <type-estimate>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Both are <type-estimate>s, so hand off
  type-estimate-disjoint?(t1, t2)
end;

///
/// These are attempts to short-circuit the typist in frequent but quick cases.
/// Metering indicates that the typist overhead was excessive for these simple,
/// frequently-occurring cases.
///

define method guaranteed-disjoint?(t1 :: <type-estimate-limited-instance>,
	                           t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton type estimates can be quickly decided here.
  ~^instance?(type-estimate-singleton(t1), t2)
end;
   
define method guaranteed-disjoint?(t1 :: <&type>,
                                   t2 :: <type-estimate-limited-instance>)
 => (res :: <boolean>)
  // Singleton type estimates can be quickly decided here.
  ~^instance?(type-estimate-singleton(t2), t1)
end;

define method guaranteed-disjoint?(t1 :: <&singleton>, t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton model types can be quickly decided here.
  ~^instance?(^singleton-object(t1), t2)
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&singleton>)
 => (res :: <boolean>)
  // Singleton model types can be quickly decided here.
  ~^instance?(^singleton-object(t2), t1)
end;

define method guaranteed-disjoint?(c1 :: <&class>, c2 :: <&class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(c1, c2)
end;

define method guaranteed-disjoint?(c1 :: <&class>, c2 :: <type-estimate-class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(c1, type-estimate-class(c2))
end;

define method guaranteed-disjoint?(c1 :: <type-estimate-class>, c2 :: <&class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(type-estimate-class(c1), c2)
end;

define method guaranteed-disjoint?(c1 :: <&class>, c2 :: <type-estimate-limited-collection>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?
    (c1, type-estimate-concrete-class(c2) | type-estimate-class(c2))
end;

define method guaranteed-disjoint?(c1 :: <type-estimate-limited-collection>, c2 :: <&class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?
    (type-estimate-concrete-class(c1) | type-estimate-class(c1), c2)
end;

define method guaranteed-disjoint?(c1 :: <type-estimate-limited-instance>,
				   c2 :: <&class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ~^instance?(type-estimate-singleton(c1), c2)
end;


define method guaranteed-disjoint?(c1 :: <type-estimate-class>, 
                                   c2 :: <type-estimate-class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(type-estimate-class(c1),type-estimate-class(c2))
end;

define method guaranteed-disjoint?(c1 :: <type-estimate-limited-collection>, 
                                   c2 :: <type-estimate-limited-collection>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  type-estimate-disjoint?(c1, c2)
end;

// I just happen to know that <top> will only occur on the right
define method guaranteed-disjoint?(c1 :: <type-estimate>, 
                                   c2 :: <&top-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-disjoint?(c1 :: <&type>, 
                                   c2 :: <&top-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-disjoint?(t1 :: <type-estimate>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  #t
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  #t
end;

///
///  potentially-joint? (<type-estimate>, <&type>) => (<boolean>)
///
///    Returns true unless all instances conforming to the type-estimate
///    are guaranteed not to be instances of the given Dylan type.
///

define function potentially-joint? 
    (type-estimate :: <type-estimate>, type :: <&type>) => (res :: <boolean>)
  // Not provably DISjoint.
  ~guaranteed-disjoint?(type-estimate, type)
end;

///
/// effectively-disjoint?(t1, t2) -- #t if any instance of t1 is unlikely 
///    to be an instance of t2. This function is only used for warnings
///    generation. No optimizations may be made based on this notion of
///    disjointness.
///

// The intuition we apply is that #f is often used as a kind of "null"
// value for the types it appears with in a type-union. Hence, whenever
// we see #f in a type-union, we remove it and compute disjointess with
// the remainder of the type-union only.

define method null-type? (t :: <type-estimate>) => (well? :: <boolean>)
  instance?(t, <type-estimate-limited-instance>)
    & type-estimate-singleton(t) == #f
end method;

define method denull-type-estimate 
    (t :: <type-estimate>) => (t :: <type-estimate>)
  t
end method;

define method denull-type-estimate 
    (t :: <type-estimate-limited-instance>) => (t :: <type-estimate>)
  if (type-estimate-singleton(t) == #f)
    make(<type-estimate-bottom>)
  else
    t
  end;
end method;

define method maybe-type-estimate-union 
    (unionees :: <list>) => (t :: <type-estimate>)
  if (empty?(unionees.tail))
    unionees.head
  else
    make(<type-estimate-union>, unionees: unionees)
  end
end method;

define method denull-type-estimate 
    (t :: <type-estimate-union>) => (t :: <type-estimate>)
  let unionees = type-estimate-unionees(t);
  if (any?(null-type?, unionees))
    local method non-null? (t :: <type-estimate>) => (well? :: <boolean>)
      ~null-type?(t)
    end method;
    let new-unionees = choose(non-null?, unionees);
    maybe-type-estimate-union(new-unionees);
  else
    t
  end;
end method;

define method union-of-values? (t :: <type-estimate>) => (well? :: <boolean>)
  instance?(t, <type-estimate-union>)
    & every?(rcurry(instance?, <type-estimate-values>), 
             type-estimate-unionees(t))
end method;

define method flatten-union-of-values 
    (t :: <type-estimate>) => (t :: <type-estimate>)
  t
end method;

define method flatten-union-of-values 
    (union :: <type-estimate-union>) => (vals :: <type-estimate-values>)
  let unionees = type-estimate-unionees(union);
  let vals1 = unionees.first;
  let n-fixed = size(type-estimate-fixed-values(vals1));
  let rest? = type-estimate-rest-values(vals1);
  let fixed-unionees-sequence
    = make(<simple-object-vector>, size: n-fixed, fill: #());
  let rest-unionees = #();
  for (vals :: <type-estimate-values> in unionees)
    for (i from 0, 
         fixed-i :: <type-estimate> in type-estimate-fixed-values(vals))
      fixed-unionees-sequence[i] 
        := add-new!(fixed-unionees-sequence[i], fixed-i);
    end;  
    if (rest?)
      rest-unionees 
        := add-new!(rest-unionees, type-estimate-rest-values(vals));
    end;
  end;
  let result = 
  make(<type-estimate-values>, 
       fixed: map(method (unionees :: <list>)
                    maybe-type-estimate-union(unionees)
                  end,
                  fixed-unionees-sequence),
       rest: rest? & maybe-type-estimate-union(rest-unionees));
  result
end method;

define method effectively-disjoint?
    (t1 :: <type-estimate>, t2 :: <type-estimate>) => (well? :: <boolean>)
  if (null-type?(t1) | null-type?(t2))
    // A bare null in a comparison stands or falls on its own merit.
    guaranteed-disjoint?(t1, t2)
  elseif (union-of-values?(t1) | union-of-values?(t2))
    effectively-disjoint?
      (flatten-union-of-values(t1), flatten-union-of-values(t2));
  else
    // When neither is just null on its own, we apply the denulling 
    // heuristic.
    let denulled-t1 = denull-type-estimate(t1);
    let denulled-t2 = denull-type-estimate(t2);
    guaranteed-disjoint?(denulled-t1, denulled-t2)
  end;
end method;

define method effectively-disjoint?
    (t1 :: <type-estimate-values>, t2 :: <type-estimate-values>)
 => (well? :: <boolean>)
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
                effectively-disjoint?(t1i, t2i) => xit(#t, #t);
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

define method effectively-disjoint?
    (t1 :: <&type>, t2 :: <&type>) => (well? :: <boolean>)
  effectively-disjoint?(as(<type-estimate>, t1), as(<type-estimate>, t2))
end method;

define method effectively-disjoint?
    (t1 :: <type-estimate>, t2 :: <&type>) => (well? :: <boolean>)
  effectively-disjoint?(t1, as(<type-estimate>, t2))
end method;

define method effectively-disjoint?
    (t1 :: <&type>, t2 :: <type-estimate>) => (well? :: <boolean>)
  effectively-disjoint?(as(<type-estimate>, t1), t2)
end method;

///
/// guaranteed-preceding-specializer? (type1, type2, type-estimate) => (boolean)
///
///    Type1 and type2 are Dylan language types that have appeared as
///    method specializers. Type-estimate is the inferred type of an
///    argument to a generic function. The predicate returns #t if type1
///    is more specific than type2 for all instances conforming to
///    type-estimate (according to the rules on page 94 of the September
///    '95 DRM). 
///
/// So, for example, in:
///
///  define class <a> (<object>) end;
///    define class <b> (<a>) end;
///    define class <c> (<a>) end;
///      define class <d> (<b>, <c>) end;
///
///  define method nurgle (o :: <a>) end;
///  define method nurgle (o :: <b>) end;
///  define method nurgle (o :: <c>) end;
///  define method nurgle (o :: <d>) end;
///
/// given a class estimate of <d> for an argument to nurgle, I should be
/// able to determine the complete sorted applicable method set, using the
/// above predicate to order the methods on <b> and <c>. And similarly if
/// the type estimate for the argument is more complex, such as an
/// intersection type estimate involving <d>. Let's hear it for
/// monotonicity!
///

define generic guaranteed-preceding-specializer?(t1  :: <type-or-est>,
                                                 t2  :: <type-or-est>,
                                                 arg :: <type-or-est>) 
  => (always-precedes? :: <boolean>);

// *** Stub for later improvement.
define method guaranteed-preceding-specializer? (type1  :: <&type>, 
                                                 type2  :: <&type>, 
                                                 arg-te :: <type-estimate>)
 => (res :: <boolean>)
  ^subtype?(type1, type2)
end;

define generic slot-fixed-offset-in (sd :: <&slot-descriptor>, 
				     te :: <type-estimate>)
    // Is this slot at fixed offset, when referenced from something of type te?
    // When this is called, we know that the slot accessor method is
    // applicable, so we only have to deal with type estimates that
    // apply to objects which could be of classes which inherit the slot.
 => (offset :: false-or(<integer>));

define method slot-fixed-offset-in (sd :: <&slot-descriptor>, 
				    te :: <type-estimate-class>)
 => (offset :: false-or(<integer>))
  // Unwrapping trampoline
  let class = type-estimate-class(te);
  slot-offset-fixed-in-class?(sd, class) &
    ^slot-fixed-offset(sd, class)
end;

define method slot-fixed-offset-in (sd :: <&slot-descriptor>, 
				    te :: <type-estimate-limited-collection>)
 => (offset :: false-or(<integer>))
  let class = type-estimate-concrete-class(te);
  slot-fixed-offset-in(sd, as(<type-estimate>, class));
end;

define method slot-fixed-offset-in (sd :: <&slot-descriptor>, 
				    te :: <type-estimate-union>)
 => (offset :: false-or(<integer>))
  // Unwrapping trampoline
  let offset = #f;
  // do all unionees have the same offset?
  // TODO:  this would be a lot simpler if unionees were a <list>
  //        or if "for" weren't so limited
  //        or if intermediate results of sequence functions were
  //           optimized away: reduce(.., map(.., ))
  block (return)
    for (te in type-estimate-unionees(te))
      let te-offset = slot-fixed-offset-in(sd, te);
      if (te-offset)
	if (offset)
	  unless (offset = te-offset)
	    return(#f);
	  end;
	else
	  offset := te-offset;
	end;
      else
	return(#f);
      end;
    end;
    offset
  end
end;

define generic slot-guaranteed-initialized-in? (sd :: <&slot-descriptor>, 
						te :: <type-estimate>)
 => (initialized? :: <boolean>);

define method slot-guaranteed-initialized-in? (sd :: <&slot-descriptor>, 
					       te :: <type-estimate-class>)
 => (initialized? :: <boolean>)
  // Unwrapping trampoline
  slot-guaranteed-initialized-in-class?(sd, type-estimate-class(te))
end;

define method slot-guaranteed-initialized-in? (sd :: <&slot-descriptor>, 
					       te :: <type-estimate-union>)
 => (initialized? :: <boolean>)
  // Unwrapping trampoline
  every?(curry(slot-guaranteed-initialized-in?, sd),
	 type-estimate-unionees(te))
end;

//// Extended entry point analysis.

// At this point we know that the call is interface-compatible with the
// generic function since this has to be true for upgrading to be
// attempted. 

// If we can prove this generic call will have a known applicable method
// chain of size one (or more some day), upgrade the call to a call to
// that method.

define variable *profile-all-calls?* = #f;
define variable *partial-dispatch?*  = #f;

define inline function call-site-caches-ok? 
    (c :: <simple-call>, f :: <&generic-function>) => (well? :: <boolean>)
  let f = lambda(environment(c));
  if (lambda-initializer?(f))
    #f
  // elseif (compiling-dylan-library?())
  //  #f
  else 
    #t
  end if;
end function;

define method maybe-upgrade-call
    (c :: <simple-call>, f :: <&generic-function>) => (res :: <boolean>)
  // The dispatch state prevents analysis being done again on a second
  // pass over the code, either due to feedback or to the call
  // computation being copied by inlining.
  // TODO: If things become more precise after inlining, allow dispatch
  // to be selectively redone by recording the previous inputs? Except
  // that CPA won't work like this anyway.
  if (dispatch-state(c) == $dispatch-untried)
    let arg-te* :: <argument-sequence> = argument-type-estimates(c);
    //  effectives is non-empty only if all methods are known:
    let effectives :: <method-sequence> = estimate-effective-methods(f, arg-te*, c);
    dispatch-state(c) := $dispatch-tried;
    // Try method upgrading
    (~empty?(effectives) & maybe-upgrade-gf-to-method-call(c, f, arg-te*, effectives))
      |
      (call-site-caches-ok?(c, f)
	 & case
	     *partial-dispatch?*
	       => maybe-upgrade-gf-to-partial-dispatch(c, f, arg-te*);
	     *profile-all-calls?*
	       => upgrade-gf-to-profiling-call-site-cache(c, f, arg-te*);
	     otherwise
	       => maybe-upgrade-gf-to-call-site-cache(c, f, arg-te*);
	   end case)
      |
      next-method()
  else
    next-method();
  end;
end;
//define method maybe-upgrade-call 
//    (c :: <simple-call>, f :: <&generic-function>) => (res :: <boolean>)
//  // The dispatch state prevents analysis being done again on a second
//  // pass over the code, either due to feedback or to the call 
//  // computation being copied by inlining.
//  // TODO: If things become more precise after inlining, allow dispatch
//  // to be selectively redone by recording the previous inputs? Except
//  // that CPA won't work like this anyway.
//  if (dispatch-state(c) == $dispatch-untried)
//    let arg-te* :: <argument-sequence> = argument-type-estimates(c);
//    let effectives :: <method-sequence> = estimate-effective-methods(f, arg-te*, c);
//    dispatch-state(c) := $dispatch-tried;
//    // Try method upgrading
//    if (*profile-all-calls?*)
//      upgrade-gf-to-profiling-call-site-cache(c, f, arg-te*)
//    else
//      (~empty?(effectives) & maybe-upgrade-gf-to-method-call(c, f, arg-te*, effectives))
//	|
//	if (*partial-dispatch?*)
//	  maybe-upgrade-gf-to-partial-dispatch(c, f, arg-te*)
//	else
//	  maybe-upgrade-gf-to-call-site-cache(c, f, arg-te*)
//	end 
//	|
//	next-method()
//    end if
//  else
//    next-method();
//  end;
//end;


define function maybe-upgrade-gf-to-method-call
    (c :: <simple-call>, f :: <&generic-function>, 
     arg-te* :: <argument-sequence>, effectives :: <method-sequence>)
 => (res :: <boolean>)
  // if we've got this far, all methods are known (== effectives)
  let effective = head(effectives);
  if (instance?(effective, <&accessor-method>))
    let slot-descriptor = get-method-slot-descriptor(effective);
    if (instance?(slot-descriptor, <&any-instance-slot-descriptor>)
	  & size(effectives) = 1) // TODO: HANDLE NEXT-METHOD 
      // gsb:  no, there is nothing to handle with next-method here.
      let arg-te = arg-te*[accessor-method-dispatch-arg(effective)];
      let offset = slot-fixed-offset-in(slot-descriptor, arg-te);
      if (offset)
	if (*colorize-dispatch*)
	  color-dispatch(c, #"slot-accessor-fixed-offset")
	end;
	incf-static-dispatch-count();
	upgrade-call-to-slot-accessor(c, effective, slot-descriptor,
				      offset, arg-te);
	#t
      else
	// the g.f. dispatcher does a better job than direct-calling
	// the accessor method, so leave it as a g.f. call
	if (*colorize-dispatch*)
	  color-dispatch(c, #"lambda-call")
	end;
	#f
      end
    else
      // if 1st of several effective methods is a slot access,
      // neither color nor upgrade
      #f
    end
  elseif (any?(rcurry(instance?, <&accessor-method>), tail(effectives)))
      #f   // ditto if any effectives are a slot access
  else
    // and if no effectives are slot accesses, upgrade to method call
    let method-call 
      = upgrade-to-method-call!
      (c, effective, tail(effectives), <method-call>);
    re-optimize(method-call); // Is this the right place to do this?
    maybe-upgrade-call(method-call, effective);
    #t
  end
end function;

/*
 * define function maybe-upgrade-gf-to-method-call
 *      (c :: <simple-call>, f :: <&generic-function>, 
 *      arg-te* :: <argument-sequence>, effectives :: <method-sequence>)
 *  => (res :: <boolean>)
 *   let effective = head(effectives);
 *   if (instance?(effective, <&accessor-method>))
 *     let slot-descriptor = get-method-slot-descriptor(effective);
 *     if (instance?(slot-descriptor, <&any-instance-slot-descriptor>)
 * 	  & size(effectives) = 1) // TODO: HANDLE NEXT-METHOD 
 *       // gsb:  no, there is nothing to handle with next-method here.
 *       let arg-te = arg-te*[accessor-method-dispatch-arg(effective)];
 *       let offset = slot-fixed-offset-in(slot-descriptor, arg-te);
 *       if (offset)
 * 	if (*colorize-dispatch*)
 * 	  color-dispatch(c, #"slot-accessor-fixed-offset")
 * 	end;
 * 	upgrade-call-to-slot-accessor(c, effective, slot-descriptor,
 * 				      offset, arg-te);
 * 	#t
 *       else
 * 	// the g.f. dispatcher does a better job than direct-calling
 * 	// the accessor method, so leave it as a g.f. call
 * 	if (*colorize-dispatch*)
 * 	  color-dispatch(c, #"failed-to-select-where-all-known")
 * 	end;
 * 	#f
 *       end
 *     else
 *       // the g.f. dispatcher does a better job than direct-calling
 *       // the accessor method, so leave it as a g.f. call
 *       if (*colorize-dispatch*)
 * 	color-dispatch(c, #"failed-to-select-where-all-known")
 *       end;
 *       #f
 *     end
 *   elseif (any?(rcurry(instance?, <&accessor-method>), tail(effectives)))
 *       if (*colorize-dispatch*)
 * 	color-dispatch(c, #"failed-to-select-where-all-known")
 *       end;
 *       #f
 *   else
 *     let method-call 
 *       = upgrade-to-method-call!
 *       (c, effective, tail(effectives), <method-call>);
 *     re-optimize(method-call); // Is this the right place to do this?
 *     maybe-upgrade-call(method-call, effective);
 *     #t
 *   end
 * end function;
 * 
 */

///
/// GF PARTIAL DISPATCH CALLS
///

define function compute-partial-dispatch-cache-mask 
    (g :: <&generic-function>, arg-te* :: <argument-sequence>, 
     call :: <simple-call>, effectives :: <method-sequence>)
 => (mask :: <integer>, arg-te* :: false-or(<argument-sequence>))
  let gsig = ^function-signature(g);
  let nargs :: <integer> = ^signature-number-required(gsig);
  if ((~empty?(effectives) & empty?(tail(effectives))) | nargs = 0)
    values(0, #f)
  else
    let nargs :: <integer> = min(nargs, $partial-dispatch-arguments-limit);
    let obj :: <&class> = dylan-value(#"<object>");
    let mask :: <integer> = 0;
    let ans :: <argument-sequence> 
      = (collecting (as <argument-sequence>)
	   for (m :: <integer> = 1 then ash(m, 1),
		arg-te in arg-te*, 
		lim :: <integer> from 0 below nargs)
	     let t = as(<&type>, arg-te);
	     unless (t == obj)
	       mask := logior(mask, m);
	       if (instance?(t, <&singleton>))
		 debug-out(#"partial-dispatch", ">>> Got singleton %= \n", ^singleton-object(t));
		 let object
		   = ^singleton-object(t);
		 let type 
		   = if (instance?(object, <&class>) | object == #f)
		       t
		     else 
		       ^object-class(object)
		     end if;
		 collect(type)
	       else 
		 debug-out(#"partial-dispatch", ">>> Got type %= \n", t);
		 collect(t)
	       end if
	     end unless;
	   end for
	end collecting);
    values(mask, mask ~== 0 & ans)
  end if
end function;

		    
define method maybe-upgrade-gf-to-partial-dispatch
    (c :: <simple-call>, f :: <&generic-function>, 
     arg-te* :: <argument-sequence>)
 => (res :: <boolean>)
  maybe-upgrade-gf-to-partial-dispatch-1(c, f, arg-te*, 0)
end method;

define method maybe-upgrade-gf-to-partial-dispatch
    (c :: <engine-node-call>, f :: <&generic-function>, 
     arg-te* :: <argument-sequence>)
 => (res :: <boolean>)
  maybe-upgrade-gf-to-partial-dispatch-1
    (c, f, arg-te*, ^pdisp-type-mask(reference-value(engine-node(c))))
end method;

define function maybe-upgrade-gf-to-partial-dispatch-1
    (c :: <simple-call>, f :: <&generic-function>, 
     arg-te* :: <argument-sequence>, premask :: <integer>)
 => (res :: <boolean>)
  
  let candidates :: <method-sequence> =
    if (all-applicable-methods-guaranteed-known?(f, arg-te*))
      let methods-known
	= ^generic-function-methods-known(f);
      let (leading-sorted, others) 
	= guaranteed-sorted-applicable-methods(methods-known, arg-te*);
      concatenate(leading-sorted, others)
    else
      #()
    end if;

  let (mask, success-types)
    = compute-partial-dispatch-cache-mask(f, arg-te*, c, candidates);

  local method collect-failure-types ()
          collecting ()
            for (i :: <integer> from 0, spec in arg-te*)
	      unless (logbit?(i, mask))
                collect(list(i, spec))
	      end
	    end;
	  end
	end method;
  if (mask == premask) // Already dealing with all possible arg positions.
    note-when(*trace-call-cache-failure*,
	      <failed-to-eliminate-partial-type-checking>,
	      source-location:       dfm-source-location(c),
	      context-id:            dfm-context-id(c),
	      failed-parameters:     collect-failure-types(),
	      failed-generic:        f.model-variable-name,
	      failed-type-estimates: arg-te*);
    #f
  else
    // let trymask :: <integer> = logand(mask, lognot(premask));
    format-when(*trace-call-cache-success*,
		"Call site cache upgrade from %d to %d on %=\n", 
		premask, mask, c);
    let engine-call 
      = upgrade-to-partial-dispatch-call!
          (c, f, mask, success-types, <engine-node-call>);
    re-optimize(engine-call);
    maybe-upgrade-call(engine-call, f);
    #t
  end if
end function;


define method maybe-wrap-profiling-engine-node
    (g :: <&generic-function>, call-site-cache :: <&cache-header-engine-node>)
 => (res :: <&cache-header-engine-node>)
  if (*profile-all-calls?*)
    // accumulated in checking phase to allow for generics to change id
    let engine-node 
      = ^make(<&profiling-call-site-cache-header-engine-node>,
	      next:     call-site-cache,
	      function: g);
    ^cache-header-engine-node-parent(call-site-cache) := engine-node;
    engine-node
  else 
    call-site-cache
  end if
end method;


define method maybe-set-profiling-engine-node-call
    (call-site-cache :: <&cache-header-engine-node>, call-c)
end method;

define method maybe-set-profiling-engine-node-call
    (call-site-cache :: <&profiling-call-site-cache-header-engine-node>, call-c)
  profiling-call-site-cache-header-engine-node-call(call-site-cache) := call-c;
end method;


define method upgrade-to-partial-dispatch-call!
    (call :: <function-call>, g :: <&generic-function>, 
     type-mask :: <integer>, types :: <argument-sequence>,
     new-call-class :: <class>)
  let env 
    = environment(call);
  let function-ref
    = make-object-reference(g);
  let call-site-cache
    = ^make(<&partial-dispatch-cache-header-engine-node> , 
	    function:  g,
	    types:     types,
	    type-mask: type-mask);
  let engine-node-ref
    = make-object-reference(maybe-wrap-profiling-engine-node(g, call-site-cache));
  let (first-c, last-c, new-arguments)
    = method-call-arguments(call, g);
  let (call-c, call-t)
    = make-with-temporary
        (env, new-call-class,
	 temporary-class: call-temporary-class(call),
	 function:        function-ref,
	 engine-node:     engine-node-ref,
	 arguments:       new-arguments);
  maybe-set-profiling-engine-node-call(reference-value(engine-node-ref), call-c);
  let (first-c, last-c) = join-2x1!(first-c, last-c, call-c);
  compatibility-state(call-c) := compatibility-state(call);
  replace-call-computation!
    (env, call, first-c, last-c, call-t);
  call-c
end method;


define method upgrade-to-profiling-call-site-cache-call!
    (call :: <function-call>, g :: <&generic-function>, 
     types :: <argument-sequence>,
     new-call-class :: <class>)
  let env 
    = environment(call);
  let function-ref
    = make-object-reference(g);
  let call-site-cache
    = ^make(<&simple-call-site-cache-header-engine-node>, function: g);
  let engine-node-ref
    = make-object-reference(maybe-wrap-profiling-engine-node(g, call-site-cache));
  let (first-c, last-c, new-arguments)
    = method-call-arguments(call, g);
  let (call-c, call-t)
    = make-with-temporary
        (env, new-call-class,
	 temporary-class: call-temporary-class(call),
	 function:        function-ref,
	 engine-node:     engine-node-ref,
	 arguments:       new-arguments);
  maybe-set-profiling-engine-node-call(reference-value(engine-node-ref), call-c);
  let (first-c, last-c) = join-2x1!(first-c, last-c, call-c);
  compatibility-state(call-c) := compatibility-state(call);
  replace-call-computation!
    (env, call, first-c, last-c, call-t);
  call-c
end method;

define method upgrade-gf-to-profiling-call-site-cache 
    (c :: <simple-call>, f :: <&generic-function>, arg-te* :: <argument-sequence>)
  let engine-call = upgrade-to-profiling-call-site-cache-call!(c, f, arg-te*, <engine-node-call>);
  re-optimize(engine-call);
  maybe-upgrade-call(engine-call, f);
  #t
end method;

///
/// GF TYPECHECKED CALLS
///

define function get-gf-typechecked-cache-mask (g :: <&generic-function>) 
 => (m :: <pair>)
  ^generic-function-cache-info(g)
    | (begin
	 let ans = compute-gf-typechecked-cache-mask(g);
	 if (^%gf-cache(g) == #f) ^%gf-cache(g) := head(ans) end;
	 ^generic-function-cache-info(g) := ans
       end)
end function;


define function compute-gf-typechecked-cache-mask (g :: <&generic-function>)
  let meths :: <method-sequence> = ^generic-function-methods(g);
  let gsig = ^function-signature(g);
  let nargs :: <integer> = ^signature-number-required(gsig);
  if (empty?(tail(meths)) | nargs < 2)
    // Don't do anything if there aren't at least two methods, or two arguments.
    #(0 . #f)
  elseif (^generic-function-sealed?(g))
    let nargs :: <integer> = min(nargs, $simple-typechecked-cache-arguments-limit);
    let meth-1 :: <&method> = head(meths);
    let spec1 = ^function-specializers(meth-1);
    let obj :: <&class> = dylan-value(#"<object>");
    let mask :: <integer> = 0;
    // Initialize the mask to those arg positions which don't have <object>
    // in them already.
    for (m :: <integer> = 1 then ash(m, 1), i from 0 below nargs)
      if (~^type-equivalent?(spec1[i], obj)) mask := logior(mask, m) end
    end for;
    // Clear the bits for we need to force dispatch for whatever reason.
    // Early-out if the positions which could be cleared, already are.
    // @@@@ This should be done more generally...
    for (meth :: <&method> in meths, until: (logand(mask, 3) == 0))
      if (instance?(meth, <&accessor-method>))
	^ensure-slots-initialized(^signature-required(^function-signature(meth))
				    [accessor-method-dispatch-arg(meth)]);
	let sd = ^method-slot-descriptor(meth);
        // (gts,98jan15) sd could be #f in the case of a duplicate slot.
        // warning will have been given, and duplicate slot removed from class' 
        // direct slots.  The method, added during parsing, lives on, though.
	if (sd & ~slot-offset-fixed-in-class?(sd, ^slot-owner(sd)))
	  mask := logand(mask, lognot(ash(1, accessor-method-dispatch-arg(meth))))
	end if
      end if
    end for;
    // Go over remaining methods, look for differing specializers.
    for (meth :: <&method> in tail(meths), until: (mask == 0))
      let spec2 = ^function-specializers(meth);
      for (i :: <integer> from 0 below nargs, m :: <integer> = 1 then ash(m, 1))
	if (logbit?(i, mask) & ~^type-equivalent?(spec1[i], spec2[i]))
	  mask := logand(mask, lognot(m))
	end if
      end for
    end for;
    format-when(*trace-call-cache-success* | *trace-call-cache-failure*,
		"Computed cache mask of %d for %=\n", mask, g);
    if (mask == 0) #(0 . #f) else pair(mask, spec1) end
  else
    #(0 . #f)
  end if
end function;

		    
define method maybe-upgrade-gf-to-call-site-cache
    (c :: <simple-call>, f :: <&generic-function>, 
     arg-te* :: <argument-sequence>)
 => (res :: <boolean>)
  maybe-upgrade-gf-to-call-site-cache-1(c, f, arg-te*, 0)
end method;

define method maybe-upgrade-gf-to-call-site-cache
    (c :: <engine-node-call>, f :: <&generic-function>, 
     arg-te* :: <argument-sequence>)
 => (res :: <boolean>)
  let e :: <&simple-typechecked-cache-header-engine-node>
    = reference-value(engine-node(c));
  maybe-upgrade-gf-to-call-site-cache-1(c, f, arg-te*, ^stchen-checkedmask(e))
end method;

define function maybe-upgrade-gf-to-call-site-cache-1
    (c :: <simple-call>, f :: <&generic-function>, 
     arg-te* :: <argument-sequence>, premask :: <integer>)
 => (res :: <boolean>)
  let stuff :: <pair> = get-gf-typechecked-cache-mask(f);
  let mask :: <integer> = head(stuff);
  if (mask == premask) // Already dealing with all possible arg positions.
    #f
  else
    let trymask :: <integer> = logand(mask, lognot(premask));
    let specs :: <simple-object-vector> = tail(stuff);
    local method frobnicate (m :: <integer>, i :: <integer>, resultm :: <integer>)
	    if (m == 0)
	      if (resultm == premask)
		note-when(*trace-call-cache-failure*,
			  <failed-to-eliminate-partial-type-checking>,
			  source-location: dfm-source-location(c),
			  context-id:      dfm-context-id(c),
			  failed-parameters: collecting ()
					       for (i :: <integer> from 0 below best-function-number-required(f), 
						    spec in best-function-specializers(f))
						 if (logbit?(i, trymask)) 
						   collect(list(i, spec))
						 end
					       end;
					     end,
			  failed-generic: f.model-variable-name,
			  failed-type-estimates: arg-te*);
		#f
	      else
		format-when(*trace-call-cache-success*,
			    "Call site cache upgrade from %d to %d on %=\n", 
			    premask, resultm, c);
		let engine-call 
		  = upgrade-to-simple-typechecked-gf-cache!
		      (c, f, resultm, <engine-node-call>);
		re-optimize(engine-call);
		maybe-upgrade-call(engine-call, f);
		#t
	      end if
	    else
	      frobnicate(ash(m, -1), i + 1,
			 if (logbit?(0, m) &
                             guaranteed-joint?(arg-te*[i], specs[i]))
			   logior(resultm, ash(1, i))
			 else
			   resultm
			 end if)
	    end if
	  end method;
    frobnicate(trymask, 0, premask)
  end if
end function;

define method maybe-upgrade-call 
    (c :: <engine-node-call>, f :: <&generic-function>) => (res :: <boolean>)
  let signature = f.^function-signature;
  let required-count = signature.^signature-number-required;
  if (dispatch-state(c) == $dispatch-untried)
    let arg-te*    = required-argument-type-estimates(c, required-count);
    let effectives = estimate-effective-methods(f, arg-te*, c);
    dispatch-state(c) := $dispatch-tried;
    if (empty?(effectives))
      next-method();
    else
      let effective = effectives.head;
      if (instance?(effective, <&accessor-method>))
	let slot-descriptor = get-method-slot-descriptor(effective);
	if (instance?(slot-descriptor, <&any-instance-slot-descriptor>)
	      & size(effectives) = 1) // TODO: HANDLE NEXT-METHOD 
	  // gsb:  no, there is nothing to handle with next-method here.
	  let arg-te = arg-te*[accessor-method-dispatch-arg(effective)];
	  let offset = slot-fixed-offset-in(slot-descriptor, arg-te);
	  if (offset)
	    if (*colorize-dispatch*)
	      color-dispatch(c, #"slot-accessor-fixed-offset")
	    end if;
	    incf-static-dispatch-count();
	    upgrade-call-to-slot-accessor(c, effective, slot-descriptor,
					  offset, arg-te);
	    #t
	  else
	    // the g.f. dispatcher does a better job than direct-calling
	    // the accessor method, so leave it as a g.f. call
	    if (*colorize-dispatch*)
	      color-dispatch(c, #"lambda-call")
	    end if;
	    #f
	  end if
	else
	  // if 1st of several effective methods is a slot access,
	  // neither color nor upgrade
	  #f
	end if
      elseif (any?(rcurry(instance?, <&accessor-method>), effectives))
	next-method();
      else
	let method-call
          = upgrade-to-method-call!
              (c, effective, effectives.tail, <method-call>);
	maybe-upgrade-call(method-call, effective);
	#t;
      end;
    end;
  else
    next-method();
  end;
end method;

define method maybe-upgrade-call 
    (c :: <apply>, f :: <&generic-function>) => (res :: <boolean>)
  // The dispatch state prevents analysis being done again on a second
  // pass over the code, either due to feedback or to the call 
  // computation being copied by inlining.
  // TODO: If things become more precise after inlining, allow dispatch
  // to be selectively redone by recording the previous inputs? Except
  // that CPA won't work like this anyway.
  let signature = f.^function-signature;
  let required-count = signature.^signature-number-required;
  if (dispatch-state(c) == $dispatch-untried
        & c.arguments.size - 1 >= required-count)
    let arg-te* = required-argument-type-estimates(c, required-count);
    let effectives = estimate-effective-methods(f, arg-te*, c);
    dispatch-state(c) := $dispatch-tried;
    if (empty?(effectives))
      next-method();
    else
      let effective = effectives.head;
      if (any?(rcurry(instance?, <&accessor-method>), effectives))
	next-method();
      else
	let method-apply
          = upgrade-to-method-call!
              (c, effective, effectives.tail, <method-apply>);
	maybe-upgrade-call(method-apply, effective);
	#t;
      end;
    end;
  else
    next-method();
  end;
end method;

// Suppose we can't statically resolve a dispatch, but one or more of the 
// arguments is a type-union.  Suppose the call to each tuple in the 
// "CPA expansion" can be statically resolved to a slot access, and the offset 
// and slot kind are identical in each case.  Then we can still statically 
// dispatch as the code will be identical in each case, even though the slots
// are semantically different.  Detecting this case may help us to implement
// methods on limited collections without having to duplicate large chunks of
// code.  The following code identifies this situation.
// This code is only temporary, as a different approach will probably be 
// required for Andy S's typist, and the following code duplicates too much of
// maybe-upgrade-call.

// Unfortunately calling estimate-effective-methods generates a whole bunch
// of spurious typing errors when I do this, and so the call to 
// maybe-upgrade-union-call is currently disabled.

// define method maybe-upgrade-union-call
//     (c :: <simple-call>, f :: <&generic-function>, arg-te* :: <argument-sequence>) 
//         => (res :: <boolean>)
// 
//   block (return)
//     let products = list(arg-te*);
// 
//     // Compute a partial CPA expansion.  We are only interested in detecting
//     // slot accesses so only expand out at most the first two arguments.
//     for (i :: <integer> from 0 below min(arg-te*.size, 2))
//       let arg-te = arg-te*[i];
//       if (instance?(arg-te, <type-estimate-union>))
//         products := 
//           reduce(method(l, te)
//                    reduce(method(l, p) 
//                             let new-p = copy-sequence(p);
//                             new-p[i] := te; 
//                             pair(new-p, l) 
//                           end, l, products)
//                  end, #(), type-estimate-unionees(arg-te));
//       end;
//     end;
// 
//     unless (products.size > 1) return (#f) end;  // There were no unions...
// 
//     local method compute-offset-if-possible(new-arg-te*)
//       let effectives = estimate-effective-methods(f, new-arg-te*, c);
//       if (effectives.size ~= 1) return(#f) end;
//       let effective = head(effectives);
//       unless (instance?(effective, <&accessor-method>)) return (#f) end;
//       let slot-descriptor = get-method-slot-descriptor(effective);
//       unless (instance?(slot-descriptor, <&any-instance-slot-descriptor>))
//         return (#f)
//       end;
//       let arg-te = arg-te*[accessor-method-dispatch-arg(effective)];
//       let offset = slot-fixed-offset-in(slot-descriptor, arg-te);
//       unless (offset) return (#f) end;
//       values(offset, slot-descriptor, arg-te, effective)
//     end;
// 
//     let (offset,sd,arg-te,effective) = compute-offset-if-possible(products[0]);
//     let slot-type = ^slot-type(sd);
//     let initialized? = slot-guaranteed-initialized-in?(sd, arg-te);
// 
//     for (i :: <integer> from 1 below products.size)
//       let (offset-i, sd-i, arg-te-i)
//         = compute-offset-if-possible(products[i]);
//       unless ( (offset-i = offset)
//              & (sd-i.object-class = sd.object-class)
//              & (^slot-type(sd-i) = slot-type)
//              & (initialized? = 
//                   slot-guaranteed-initialized-in?(sd-i, arg-te-i)))
//         return (#f)
//       end
//     end;
// 
//     if (*colorize-dispatch*)
//       color-dispatch(c, #"slot-accessor-fixed-offset")
//     end;
// 
//     incf-static-dispatch-count();
//     upgrade-call-to-slot-accessor(c, effective, sd, offset, arg-te);
//     #t;
//   end
// end;        

define method upgrade-call-to-slot-accessor
    (c :: <simple-call>, m :: <&getter-method>, 
     sd :: <&instance-slot-descriptor>, offset :: <integer>,
     te :: <type-estimate>)
 => ()
  let env = environment(c);
  let args = arguments(c);

  let (sv-c, sv-t)
    = make-with-temporary
        (env, <slot-value>,
         guaranteed-initialized?: slot-guaranteed-initialized-in?(sd, te),
         slot-descriptor: sd,
         slot-offset: offset,
         instance: args[0]);

  re-optimize(sv-c);
  replace-call-computation!(env, c, sv-c, sv-c, sv-t);
end;

define method upgrade-call-to-slot-accessor
    (c :: <simple-call>, m :: <&setter-method>,
     sd :: <&instance-slot-descriptor>, offset :: <integer>,
     te :: <type-estimate>)
 => ()
  let env = environment(c);
  let args = arguments(c);
  let new-value-t = args[0];

  let (sv-c, sv-t)
    = make-with-temporary
        (env, <slot-value-setter>,
         slot-descriptor: sd,
         slot-offset: offset,
         new-value: new-value-t,
         instance: args[1]);

  replace-call-computation!(env, c, sv-c, sv-c, new-value-t)
end;

define compiler-sideways method computation-repeated-byte? 
    (c :: <any-repeated-slot-value>) => (res :: <boolean>)
  repeated-representation-byte?
    (^slot-type(computation-slot-descriptor(c)))
end method;

define method convert-boxer-call
    (env :: <lambda-lexical-environment>, function :: <&primitive>, 
     arguments :: <argument-sequence>)
  make-with-temporary
    (env, <primitive-call>, primitive: function, arguments: arguments);
end method;

define method convert-boxer-call
    (env :: <lambda-lexical-environment>, function :: <&function>, 
     arguments :: <argument-sequence>)
  let function-ref = make-object-reference(function);
  let (call-c, call-t) = 
    make-with-temporary
      (env, <simple-call>, function: function-ref, arguments: arguments);
  re-optimize(call-c);
  values(call-c, call-t)
end method;

define method maybe-convert-unbox 
    (env :: <lambda-lexical-environment>, 
     ref :: <value-reference>, type :: <&type>)
 => (unboxer-c :: false-or(<computation>), unboxed-ref :: <value-reference>)
  let (raw-rep-type?, boxer, unboxer) =
    raw-repeated-representation?(type);
  let (unboxer-c, unboxed-ref) =
    if (raw-rep-type?)
      convert-boxer-call(env, unboxer, vector(ref))
    else
      values(#f, ref)
    end if;
  values(unboxer-c, unboxed-ref)
end method;

define method maybe-convert-box 
    (env :: <lambda-lexical-environment>, 
     ref :: <value-reference>, type :: <&type>)
 => (boxer-c :: false-or(<computation>), boxed-ref :: <temporary>)
  let (raw-rep-type?, boxer) =
    raw-repeated-representation?(type);
  let (boxer-c, boxed-ref) =
    if (raw-rep-type?)
      convert-boxer-call(env, boxer, vector(ref))
    else
      values(#f, ref)
    end if;
  values(boxer-c, boxed-ref)
end method;

define method upgrade-call-to-slot-accessor
    (c :: <simple-call>, m :: <&repeated-getter-method>,
     sd :: <&repeated-slot-descriptor>, offset :: <integer>,
     te :: <type-estimate>)
 => ()
  let env = environment(c);
  let args = arguments(c);

  let function 
    = make-dylan-reference(#"interpret-integer-as-raw");
  let (raw-index-c, raw-index-t) =
    make-with-temporary
      (env, <simple-call>, function: function, arguments: vector(args[1]));
  
  let (sv-c, sv-t)
    = make-with-temporary
        (env, <repeated-slot-value>, 
         guaranteed-initialized?: slot-guaranteed-initialized-in?(sd, te),
         index-tagged?: #t,
         slot-descriptor: sd,
         slot-offset: offset,
         index: raw-index-t,
         instance: args[0]);

  let (first-c, last-c)
    = join-1x1!(raw-index-c, sv-c);

  let (boxer-c, boxed-t)
    = maybe-convert-box(env, sv-t, ^slot-type(sd));

  let (first-c, last-c, typed-value-t) =
    if (instance?(type, <&limited-integer>))
      // For limited integer slots we add an extra guarantee type because
      // the shift-n-tag analyser doesn't appear to be smart enough to
      // work for itself that the value is still in range.
      let (first-int-c, last-int-c, int-t)
        = join-2x1-t!(first-c, last-c, boxer-c, boxed-t);
      let (gt-c, gt-t) 
        = make-with-temporary
            (env, <guarantee-type>,
             value: boxed-t, 
             type:  make-object-reference(type));
      join-2x1-t!(first-int-c, last-int-c, gt-c, gt-t);
    else
      join-2x1-t!(first-c, last-c, boxer-c, boxed-t);
    end;

  re-optimize(sv-c);
  re-optimize(raw-index-c);
  replace-call-computation!
    (env, c, first-c, last-c, typed-value-t);
end;

define method upgrade-call-to-slot-accessor
    (c :: <simple-call>, m :: <&repeated-setter-method>,
     sd :: <&repeated-slot-descriptor>, offset :: <integer>,
     te :: <type-estimate>)
 => ()
  let env = environment(c);
  let args = arguments(c);
  let new-value-t = args[0];

  let function 
    = make-dylan-reference(#"interpret-integer-as-raw");
  let (raw-index-c, raw-index-t) =
    make-with-temporary
      (env, <simple-call>, function: function, arguments: vector(args[2]));

  let (unboxer-c, unboxed-t)
    = maybe-convert-unbox(env, new-value-t, ^slot-type(sd));

  let (first-c, last-c, coerced-value-t) =
    join-1x1-t!(raw-index-c, unboxer-c, unboxed-t);

  let (sv-c, sv-t)
    = make-with-temporary
        (env, <repeated-slot-value-setter>, 
         slot-descriptor: sd,
         slot-offset: offset,
         index-tagged?: #t,
         new-value: coerced-value-t,
         index: raw-index-t,
         instance: args[1]);

  let (first-c, last-c)
    = join-2x1!(first-c, last-c, sv-c);

  re-optimize(raw-index-c);
  replace-call-computation!
    (env, c, first-c, last-c, new-value-t);
end;

define method argument-type-estimates
    (c :: <function-call>) => (estimates :: <argument-sequence>)
  // gts-debug("errs", "arg-tes(funcall): c.args=%=.\n", c.arguments);
  map-as(<argument-sequence>, type-estimate, c.arguments);
end;

define method argument-type-estimates
    (c :: <apply>) => (estimates :: <argument-sequence>)
next-method();
/*
  local method flatten-apply-args (args)
          if (size(args) <= 1)
            args
          else
            let flattened = copy-sequence(args, end: size(args) - 2);
            concatentate(flattened, last(args));
          end if;
        end method;
  // gts-debug("errs", "arg-tes(apply): c.args=%=.\n", c.arguments);
  map-as(<argument-sequence>, type-estimate, flatten-apply-args(c.arguments));
*/
end;


define function required-argument-type-estimates 
    (c :: <function-call>, required-count :: <integer>)
 => (estimates :: <argument-sequence>)
  collecting (as <argument-sequence>)
    for (i :: <integer> from 0 below required-count, arg in c.arguments)
      collect(type-estimate(arg))
    end;
  end;
end;

// Compute a reference to the given function and destructively modify the
// call to refer to it rather than to what it currently refers to.

define method simplify-call-to-call-to-object! 
    (call :: <function-call>, f :: <&function>) => ()
  let ref-temp = make-object-reference(f);
  replace-call-function!(call, ref-temp);
end method;

define method method-call-arguments
    (call :: <simple-call>, func :: <&lambda>) 
 => (first-c :: false-or(<computation>), last-c :: false-or(<computation>),
     arguments :: <argument-sequence>)
  congruent-style-call-arguments(call, func)
end method;

define method method-call-arguments
    (call :: <simple-call>, func :: <&generic-function>)
 => (first-c :: false-or(<computation>), last-c :: false-or(<computation>),
     arguments :: <argument-sequence>)
  congruent-style-call-arguments(call, func)
end method;

define function congruent-style-call-arguments
    (call :: <simple-call>, func :: <&function>)
 => (first-c :: false-or(<computation>), last-c :: false-or(<computation>),
     arguments :: <argument-sequence>)
  if (~call-congruent?(call) & best-function-optionals?(func))
    let number-required =
      best-function-number-required(func);
    if (number-required = call.arguments.size) 
      let new-arguments = make(<vector>, size: number-required + 1);
      for (i :: <integer> from 0 below number-required)
        new-arguments[i] := call.arguments[i];
      end for;
      let rest-t = make-object-reference(#[]);
      new-arguments[number-required] := rest-t;
      add-user!(rest-t, call);
      values(#f, #f, new-arguments)
    else  
      let (rest-c, rest-t)
        = generate-stack-vector
            (call.environment, 
             copy-sequence(call.arguments, start: number-required));
      let new-arguments = make(<vector>, size: number-required + 1);
      for (i :: <integer> from 0 below number-required)
        new-arguments[i] := call.arguments[i];
      end for;
      new-arguments[number-required] := rest-t;
      values(rest-c, rest-c, new-arguments)
    end
  else
    values(#f, #f, arguments(call))
  end if;
end function;

define method method-call-arguments
    (call :: <apply>, func :: <&lambda>) 
 => (first-c :: false-or(<computation>), last-c :: false-or(<computation>),
     arguments :: <argument-sequence>)
  values(#f, #f, arguments(call))
end method;

define method method-call-arguments
    (call :: <engine-node-call>, func :: <&lambda>) 
 => (first-c :: false-or(<computation>), last-c :: false-or(<computation>),
     arguments :: <argument-sequence>)
  values(#f, #f, arguments(call))
end method;

define method replace-call-function! 
    (call :: <function-call>, temp :: <value-reference>) => ()
  remove-user!(call.function, call);
  add-user!(temp, call);
  call.function := temp
end method;

define function incf-static-dispatch-count ()
  let l    = current-library-description();
  let lib  = language-definition(model-library(l));
  let &lib = namespace-model(lib);
  ^library-number-static-dispatches(&lib)
    := ^library-number-static-dispatches(&lib) + 1;
end function;

define function incf-dynamic-dispatch-count ()
  let l    = current-library-description();
  let lib  = language-definition(model-library(l));
  let &lib = namespace-model(lib);
  ^library-number-dynamic-dispatches(&lib)
    := ^library-number-dynamic-dispatches(&lib) + 1;
end function;

define method upgrade-to-method-call! 
    (call :: <function-call>, f :: <&function>, next-methods :: <method-sequence>,
     new-call-class :: <class>)
 => (method-call :: <function-call>)
  let env = environment(call);
  let function-ref 
    = make-object-reference(f);
  let next-methods-ref :: <immutable-object-reference>
    = make-value-reference(next-methods, <immutable-object-reference>);
  let (first-c, last-c, new-arguments)
    = method-call-arguments(call, f);
  let (call-c, call-t)
    = make-with-temporary
        (env, new-call-class, 
         temporary-class: call-temporary-class(call),
         function:        function-ref,
         next-methods:    next-methods-ref,
         arguments:       new-arguments);
  let (first-c, last-c) = join-2x1!(first-c, last-c, call-c);
  incf-static-dispatch-count();
  compatibility-state(call-c) := compatibility-state(call);
  replace-call-computation!
    (env, call, first-c, last-c, call-t);
  call-c
end method;

define method upgrade-to-simple-typechecked-gf-cache!
    (call :: <function-call>, g :: <&generic-function>, 
     mask :: <integer>, new-call-class :: <class>)
  let env = environment(call);
  let function-ref 
    = make-object-reference(g);
  let engine-node-ref
    = make-object-reference
        (^make(<&simple-typechecked-cache-header-engine-node>,
	       function: g,
	       checkedmask: mask));
  let (first-c, last-c, new-arguments)
    = method-call-arguments(call, g);
  let (call-c, call-t)
    = make-with-temporary
        (env, new-call-class,
	 temporary-class: call-temporary-class(call),
	 function: function-ref,
	 engine-node: engine-node-ref,
	 arguments: new-arguments);
  let (first-c, last-c) = join-2x1!(first-c, last-c, call-c);
  compatibility-state(call-c) := compatibility-state(call);
  replace-call-computation!
    (env, call, first-c, last-c, call-t);
  call-c
end method;



//// Call consistency checking.

define program-warning <non-function-in-call>
  slot condition-type-estimate,
    required-init-keyword: type-estimate:;
  format-string 
    "Function value in call is not a function - inferred type is %s.";
  format-arguments type-estimate;
end program-warning;

// Check the call if it hasn't been checked already.

define method maybe-check-function-call
    (c :: <function-call>) => (ok-to-analyse?)
  // TODO: CORRECTNESS: How come this function gets called in iep form at
  // all??
  if (call-congruent?(c))
    #t
  else
    // The compatibility state prevents a call from being check this way
    // more than once.
    let state = c.compatibility-state;
    select (state)
      $compatibility-checked-compatible 
        => #t;
      $compatibility-checked-incompatible
        => #f;
      $compatibility-unchecked
        => let ok? = check-function-call(c);
           c.compatibility-state 
             := if (ok?) 
                  $compatibility-checked-compatible
                else
                  $compatibility-checked-incompatible
                end;
           ok?;
    end;
  end;
end method;

// Do a conservative check of as many things about this call as we 
// possibly can. It's conservative in the sense that it warns only
// if there's guaranteed to be a problem. If we work out ways of
// extending the language appropriately so that we don't get 
// swamped with information, a mode conservative the other way
// would be very useful.

define method check-function-call (c :: <function-call>) => (ok-to-analyse?)
  block (return)
    let function-temp = c.function;
    let function-object = constant-value(function-temp);
    if (instance?(function-object, <&code>)) 
      // We must already have checked to have upgraded this far.
      return(#t) 
    end;
    let function-te = type-estimate(function-temp);
    // If we've hit bottom, there's been an error already.   gts,98mar25
    // Just bail out.
    if (instance?(function-te, <type-estimate-bottom>))
      return(#f)
    end if;
    if (guaranteed-disjoint?(function-te, dylan-value(#"<function>")))
      note(<non-function-in-call>,
           source-location: dfm-source-location(c),
           context-id:      dfm-context-id(c),
           type-estimate:   function-te);
      return(#f);
    end;
    // TODO: We could actually use the type estimate to check the validity of 
    // the arguments in the absence of knowing the function itself.
    if (~function-object)
      return(#f);
    end;
    // So we have the object, which we know to be a function because of the
    // disjointness test on its type above. If the arguments are compatible,
    // we win.
    // **** we have only tested disjointness, not inclusion, so
    //      we don't know it's a function ****
    // TODO: Except that we don't for some reason - <unknown> gets through
    // somehow!
    if (instance?(function-object, <&function>))
      let arg-te* = argument-type-estimates(c);
      // gts-debug("errs", "check-funcall: c=%=, arg-te*=%=.\n", c, arg-te*);
      check-maybe-mv-call-compatibility(function-object, c, arg-te*);
    else
      #f
    end;
  end;
end method;

// TODO: I haven't put much effort into getting multiple values right,
// since we're moving away from representing them as multiple value
// calls.

define method check-maybe-mv-call-compatibility
    (f :: <&function>, call :: <function-call>, arg-te* :: <argument-sequence>)
 => (compatible?)
  check-call-compatibility(f, call, arg-te*);
end method;

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
    "Unknown keyword in call to %s - %s supplied, %s recognized";
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

define program-warning <argument-type-mismatch-in-call> (<incompatible-call>)
  slot condition-required-type,
    required-init-keyword: required-type:;
  slot condition-supplied-type-estimate,
    required-init-keyword: supplied-type-estimate:;
  slot condition-arg-name,                         // gts
    required-init-keyword: arg:;                   // gts
  format-string 
    "Invalid type for argument %s in call to %s:  %s supplied, %s expected.";
  format-arguments 
    arg, function, supplied-type-estimate, required-type;
end program-warning;

define program-warning <values-argument-type-mismatch-in-call> 
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

// The result value of this function is used to decide whether a call
// can be upgraded or not, so while the warnings generated err on one
// side of caution (i.e. only warn when something is known for sure
// to be wrong), the result must be conservative the other way (i.e.
// only return true if we know for certain that the input types are
// compatible).

define inline function maybe-trim-sig-types 
    (v :: <argument-sequence>, n :: <integer>) 
 => (res :: <argument-sequence>)
  if (n = size(v)) v else copy-sequence(v, end: n) end if
end function;

// rest-type-estimate seems never sent (i.e. always defaults to #f).  (gts, 10/97)
define method check-call-compatibility
    (f :: <&function>, call :: <function-call>, arg-te* :: <argument-sequence>, 
       #key rest-type-estimate = #f)
 => (compatible?);
  //  let signature = ^function-signature(f);
  let signature
    // Special kludge for raw slot accessors, whose generic gets created
    // with <object> in all specializers, but really should be the raw...
    = (instance?(f, <&generic-function>)
	 & ^generic-function-sealed?(f)
	 & begin
	     let methods = ^generic-function-methods-known(f);
	     methods.size == 1 & methods.first.^function-signature
	   end)
      | ^function-signature(f);
  block (return)
    let required-types = ^signature-required(signature);
    let required-count = ^signature-number-required(signature);
    let supplied-count = size(arg-te*);
    if (supplied-count < required-count & ~rest-type-estimate)
      // We warn on too few arguments, even in the multiple value
      // call case.
      note(<too-few-arguments-in-call>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           function: f,
           required-count: required-count,
           supplied-count: supplied-count);
      return(#f);
    end;
    let accepts-keys? 
      = ^signature-key?(signature) | ^signature-all-keys?(signature);
    let accepts-rest?
      = ^signature-rest?(signature);
    let fixed-number-required? 
      = ~(accepts-keys? | accepts-rest?);
    if (fixed-number-required? 
          & supplied-count > required-count)
      note(<too-many-arguments-in-call>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           function: f,
           required-count: required-count,
           supplied-count: supplied-count);
      return(#f);
    end;
    if (accepts-keys? & ~rest-type-estimate)
      let fixed-count
        = if (instance?(call, <method-call>)) 
            required-count + 1 
          else
            required-count
          end;
      let key-arg-count = supplied-count - fixed-count;
      if (odd?(key-arg-count))
        note(<unbalanced-keyword-arguments-in-call>,
             source-location: dfm-source-location(call),
             context-id:      dfm-context-id(call),
             function: f,
             keyword-supplied-count: key-arg-count);
        // break("Unbalanced!");
        return(#f);
      end;
      let key-arg-te* = copy-sequence(arg-te*, start: fixed-count);
      let symbol-class = dylan-value(#"<symbol>");
      collecting (non-symbol-te*)
        for (i :: <integer> from 0 below key-arg-count by 2)
          let key-te = key-arg-te*[i];
          if (guaranteed-disjoint?(key-te, symbol-class))
            collect-into(non-symbol-te*, key-te);
          end;
        end;
        let non-symbol-te* = collected(non-symbol-te*);
        if (~empty?(non-symbol-te*))
          note(<non-keywords-in-call>,
               source-location: dfm-source-location(call),
               context-id:      dfm-context-id(call),
               function: f,
               supplied-keyword-type-estimates: non-symbol-te*);
          // break("Non-symbol!");
          return(#f);
        end;
      end;
    end;
    let guaranteed-compatible? = #t;
    let guaranteed-incompatible? = #f;
    for (arg-te in arg-te*, required-type in required-types, 
         i :: <integer> from 0 below required-count)
      if (~guaranteed-joint?(arg-te, required-type))
        guaranteed-compatible? := #f;
        // if (guaranteed-disjoint?(arg-te, required-type))
        if (effectively-disjoint?(arg-te, required-type))
	  guaranteed-incompatible? := #t;
          let required-specs 
            = spec-argument-required-variable-specs(signature-spec(f));
          note(<argument-type-mismatch-in-call>,
               source-location: dfm-source-location(call),
               context-id:      dfm-context-id(call),
               function: f,
               required-type:  required-type,
               supplied-type-estimate: arg-te,
	       arg: spec-variable-name(required-specs[i]));
	end if;
      end if;
    end for;
    if (guaranteed-incompatible?)
      return(#f);
    end if;

    if (rest-type-estimate)
      for (i :: <integer> from supplied-count below required-count)
        let required-type = required-types[i];
        if (~guaranteed-joint?(rest-type-estimate, required-type))
          guaranteed-compatible? := #f;
          // if (guaranteed-disjoint?(rest-type-estimate, required-type))
          if (effectively-disjoint?(rest-type-estimate, required-type))
            note(<values-argument-type-mismatch-in-call>,
                 source-location: dfm-source-location(call),
                 context-id:      dfm-context-id(call),
                 function: f,
                 required-types: 
                   copy-sequence(required-types, start: supplied-count),
                 supplied-type-estimate: rest-type-estimate);
            return(#f);
          end;
        end;
      end;
    end;
    // TODO: CORRECTNESS: Remove this hack, its contol flags, and its
    // associated condition below.
    if (~guaranteed-compatible? 
         & instance?(f, <&method>)
         & compiling-dylan-library?())
       if (*warn-about-bogus-upgrades*)
         note(<bogus-upgrade-possible>,
              source-location: dfm-source-location(call),
              context-id:      dfm-context-id(call),
              function: f,
              required-types:  maybe-trim-sig-types
		                 (required-types, required-count),
              supplied-type-estimates: arg-te*);
       end;
       if (*colorize-bogus-upgrades*)
         color-dispatch(call, #"bogus-upgrade");
       end;
       // Fake it in the Dylan library.
       #t
    else
      guaranteed-compatible?
    end;
  end block;
end method;

define program-warning <bogus-upgrade-possible> (<incompatible-call>)
  slot condition-required-types,
    required-init-keyword: required-types:;
  slot condition-supplied-type-estimates,
    required-init-keyword: supplied-type-estimates:;
  format-string 
    "Bogus call upgrade possible for call to %s - %s supplied and considered compatible, %s expected.";
  format-arguments 
    function, supplied-type-estimates, required-types;
end program-warning;

// For bare methods we can check keyword compatibility too.

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

define method check-call-compatibility
    (f :: <&method>, call :: <function-call>, arg-te* :: <argument-sequence>, 
     #key rest-type-estimate = #f)
 => (compatible?)
  let compatible? = next-method();
  if (~compatible?)
    #f
  else
    // Do any extra checking possible for bare method calls.
    block (return)
      let signature = ^function-signature(f);
      if (~rest-type-estimate 
            & ^signature-key?(signature) 
            & ~ ^signature-all-keys?(signature))
        let recognized-keys = ^signature-keys(signature);
        collecting (unrecognized-keys)
          let arg-temps = call.arguments;
          let arg-count = size(arg-temps);
          let key-start = ^signature-number-required(signature);
          for (i :: <integer> from key-start below arg-count by 2)
            // TODO: I'd like to do this using the type estimate, but it 
            // doesn't currently have singleton precision.
            // TODO: Check the declared types of the keyword in the method
            // being called here too.
            let (arg-value-constant?, arg-value) =
	      constant-value?(arg-temps[i]);
            if (arg-value-constant?
                  & ^symbol?(arg-value)
                  & ~member?(arg-value, recognized-keys))
              collect-into(unrecognized-keys, arg-value);
            end;
          end;
          let unrecognized-keys = collected(unrecognized-keys);
          if (~empty?(unrecognized-keys))
            note(<unrecognized-keyword-arguments-in-call>,
                 source-location:  dfm-source-location(call),
                 context-id:       dfm-context-id(call),
                 function:         f,
                 supplied-keywords: unrecognized-keys,
                 recognized-keywords: recognized-keys);
            return(#f);
          end;
        end;
      end;
      #t;
    end block;
  end if;
end method;

// We do what checking we can for apply. We should really be able to
// do pretty much everything with any positionally supplied arguments.
// So, f(x, y, z) and apply(f, x, y, z, #[]) should be checked to
// a similar level of accuracy, including checking non-symbol
// keyword arguments and the like.

// TODO: Do better. Reuse more code, but keep the more specific error
// reporting.

define program-warning <too-many-arguments-in-apply-call> 
    (<argument-count-mismatch-in-call>)
  format-string 
    "Too many arguments in application of %s - "
    "%s supplied positionally to apply, only %s expected.";
end program-warning;

define program-warning <argument-type-mismatch-in-apply-call> 
    (<incompatible-call>)
  slot condition-required-type,
    required-init-keyword: required-type:;
  slot condition-supplied-type-estimate,
    required-init-keyword: supplied-type-estimate:;
  slot condition-arg-name,                         // gts
    required-init-keyword: arg:;                   // gts
  format-string 
    "Invalid type for argument %s in application of %s: "  // gts
    "type %s supplied, but %s expected.";
  format-arguments 
    arg, function, supplied-type-estimate, required-type;
end program-warning;

define method check-call-compatibility
    (f :: <&function>, call :: <apply>, arg-te* :: <argument-sequence>, 
       #key rest-type-estimate = #f)
 => (compatible?)
  check-apply-compatibility
    (f, call, arg-te*, rest-type-estimate: rest-type-estimate);
end method;

define method check-call-compatibility
    (f :: <&method>, call :: <apply>, arg-te* :: <argument-sequence>, 
       #key rest-type-estimate = #f)
 => (compatible?)
  check-apply-compatibility
    (f, call, arg-te*, rest-type-estimate: rest-type-estimate);
end method;

define method check-apply-compatibility
    (f :: <&function>, call :: <apply>, arg-te* :: <argument-sequence>, 
       #key rest-type-estimate = #f)
 => (compatible?)
  let arg-te* = copy-sequence(arg-te*, end: arg-te*.size - 1);
  let signature = ^function-signature(f);
  // gts-debug("errs", "check-apply-compat:  call=%=, f=%=, arg-te*=%=.\n",
  //           call, f, arg-te*);
  block (return)
    let required-types = ^signature-required(signature);
    let required-count = ^signature-number-required(signature);
    let supplied-count = size(arg-te*);
    let accepts-keys? 
      = ^signature-key?(signature) | ^signature-all-keys?(signature);
    let accepts-rest?
      = ^signature-rest?(signature);
    let fixed-number-required? 
      = ~(accepts-keys? | accepts-rest?);
    if (fixed-number-required? 
          & supplied-count > required-count)
      note(<too-many-arguments-in-apply-call>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           function: f,
           required-count: required-count,
           supplied-count: supplied-count);
      return(#f);
    end;
    let guaranteed-compatible? = #t;
    let guaranteed-incompatible? = #f;
    let intersect-count = min(required-count, supplied-count);
    for (arg-te in arg-te*, required-type in required-types, 
         i :: <integer> from 0 below required-count)
      if (~guaranteed-joint?(arg-te, required-type))
        guaranteed-compatible? := #f;
        // if (guaranteed-disjoint?(arg-te, required-type))
        if (effectively-disjoint?(arg-te, required-type))
	  guaranteed-incompatible? := #t;
          note(<argument-type-mismatch-in-apply-call>,
               source-location: dfm-source-location(call),
               context-id:      dfm-context-id(call),
               function: f,
               required-type: required-type,
               supplied-type-estimate: arg-te,
	       arg: best-function-specializers(f)[i]);
	end if;
      end if;
    end for;
    if (guaranteed-incompatible?)
      return (#f);
    end if;
    // TODO: CORRECTNESS: Remove this hack, its contol flags, and its
    // associated condition below.
    if (~guaranteed-compatible? 
         & instance?(f, <&method>)
	 & compiling-dylan-library?())
       if (*warn-about-bogus-upgrades*)
         note(<bogus-apply-upgrade-possible>,
              source-location: dfm-source-location(call),
              context-id:      dfm-context-id(call),
              function: f,
              required-types: 
                 copy-sequence(required-types, end: intersect-count),
              supplied-type-estimates: arg-te*);
       end;
       if (*colorize-bogus-upgrades*)
         color-dispatch(call, #"bogus-upgrade");
       end;
       // Fake it in the Dylan library.
       #t
    else
      guaranteed-compatible?
    end;
  end;
end method;

define program-warning <bogus-apply-upgrade-possible>
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

//// Compile-time method selection.

define program-warning <no-applicable-methods-in-call> (<incompatible-call>)
  slot condition-supplied-type-estimates,
    required-init-keyword: supplied-type-estimates:;
  format-string 
    "No applicable methods for call to %s - inferred argument types %s.";
  format-arguments 
    function, supplied-type-estimates;
end program-warning;

// TODO: Write a proper optimization note framework to use here and 
// elsewhere.

define performance-note <optimization-note> end;

define performance-note <selected-where-all-known> (<optimization-note>)
  slot condition-selected-method,
    required-init-keyword: selected-method:;
  slot condition-type-estimates,
    required-init-keyword: type-estimates:;
  format-string "Selected method %s on %s.";
  format-arguments selected-method, type-estimates;
end performance-note;

define performance-note <failed-to-select-where-all-known> (<optimization-note>)
  slot condition-failed-generic,
    required-init-keyword: failed-generic:;
  slot condition-failed-type-estimates,
    required-init-keyword: failed-type-estimates:;
  format-string "Selection failed for %s on %s.";
  format-arguments failed-generic, failed-type-estimates;
end performance-note;

define performance-note <not-all-methods-known> (<optimization-note>)
  slot condition-failed-generic,
    required-init-keyword: failed-generic:;
  slot condition-failed-type-estimates,
    required-init-keyword: failed-type-estimates:;
  format-string "Can't dispatch %s on %s.";
  format-arguments failed-generic, failed-type-estimates;
end performance-note;

define performance-note <failed-to-eliminate-partial-type-checking> (<optimization-note>)
  slot condition-failed-generic,
    required-init-keyword: failed-generic:;
  slot condition-failed-type-estimates,
    required-init-keyword: failed-type-estimates:;
  slot condition-failed-parameters,
    required-init-keyword: failed-parameters:;
  format-string "Failed to eliminate type checking for args %= in call to %s with %s.";
  format-arguments failed-parameters, failed-generic, failed-type-estimates;
end performance-note;


define method color-dispatch (call :: <function-call>, decision)
  let loc = dfm-source-location(call);
  if (loc)
    color-location (loc, decision)
  end;
end method;

define method color-location (loc :: <compiler-range-source-location>, decision)
  // TODO: should verify that loc is within current form.
  let cr = loc.source-location-record;
  if (current-library-description?(cr.compilation-record-library))
    let decisions = cr.compilation-record-dispatch-decisions;
    // dispatch decisions might already have been canonicalized into a vector
    // if the library has already been fully compiled previously, but now
    // we've re-entered it in order to lazily (re)compute a model [see bug
    // #1000342 for a test case]. In that case, just don't record the
    // decision at all.
    when (instance?(decisions, <list>))
      cr.compilation-record-dispatch-decisions :=
	pair(vector(loc.source-location-start-offset,
		    loc.source-location-end-offset,
		    decision),
	     decisions);
    end;
  end;
end;

define method estimate-effective-methods
    (f :: <&generic-function>, arg-te* :: <argument-sequence>, 
     call-or-method :: type-union(<function-call>, <&method>))
 => (effectives :: <method-sequence>)
  estimate-effective-methods-from-scratch(f, arg-te*, call-or-method);
  /*
  let n-required = ^signature-number-required(^function-signature(f));
  let arg-te* = if (size(arg-te*) == n-required) 
                  arg-te*
                else
                  copy-sequence(arg-te*, end: n-required);
                end;
  let methods = type-estimate-dispatch-cache-lookup(f, arg-te*);
  if (methods)
    methods
  else
    let methods
      = estimate-effective-methods-from-scratch(f, arg-te*, call-or-method);
    add-type-estimate-dispatch-cache-entry(f, arg-te*, methods);
    methods
  end;
  */
end method;

define method estimate-effective-methods-from-scratch
    (f :: <&generic-function>, arg-te* :: <argument-sequence>, 
    call-or-method :: type-union(<function-call>, <&method>))
 => (effectives :: <method-sequence>)
  let (call, lambda) 
     = if (instance?(call-or-method, <function-call>))
         values(call-or-method, #f)
       else
         values(#f, call-or-method)
       end if;
  if (all-applicable-methods-guaranteed-known?(f, arg-te*))
    format-when(*trace-dispatch*, "Could dispatch: %= on %=\n", f, arg-te*);
    let methods-known
      = ^generic-function-methods-known(f);
    let methods
      = if (call)
          methods-known
        else
          // Lose all methods that are known statically always to be more 
          // specific than ourselves, leaving only methods known to be
          // less specific and those that are potentially more or less 
          // specific.
          choose(method (them :: <&method>) 
                   them == lambda 
                     | ~guaranteed-method-precedes?(them, lambda, arg-te*)
                 end method,
                 methods-known)
        end if;
    let (leading-sorted, others) 
      = guaranteed-sorted-applicable-methods(methods, arg-te*);

    if (empty?(leading-sorted) & empty?(others))
      note-when
	(call,
	 <no-applicable-methods-in-call>,
	 source-location: dfm-source-location(call),
	 context-id:      dfm-context-id(call),
	 function:        f,
	 supplied-type-estimates: arg-te*);
      #()
    else
      let chain
        = guaranteed-complete-method-chain(leading-sorted, others, arg-te*);

      if (chain)
	if (call)
	  maybe-note
	    (<selected-where-all-known>,
	     source-location: dfm-source-location(call),
	     context-id:      dfm-context-id(call),
	     selected-method: chain.first.model-definition,
	     type-estimates:  arg-te*);

	  if (*colorize-dispatch* & empty?(chain))
	    color-dispatch(call, #"failed-to-select-where-all-known")
	  end;
        else
          // Prune out everything that precedes us in the next method
          // chain. Note that the method calling next-method might not
          // actually be on this list if more specific methods override
          // entirely (i.e. don't call next-method) because of the way
          // the chain is constructed.
          for (cursor :: <list> = chain then cursor.tail, 
                 until: empty?(cursor) | cursor.head == lambda)
          finally
            cursor
          end;
	end if;

        chain
      else
        if (call & *colorize-dispatch*)
          color-dispatch(call, #"failed-to-select-where-all-known")
        end;

        note-when
          (call & *trace-selection-failure*,
           <failed-to-select-where-all-known>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           failed-generic: f.model-variable-name,
           failed-type-estimates: arg-te*);
        #()
      end;
    end;
  else
    if (call & *colorize-dispatch*)
      color-dispatch(call, #"not-all-methods-known")
    end;

    note-when
      (call & *trace-dispatch-failure*,
       <not-all-methods-known>,
       source-location: dfm-source-location(call),
       context-id:      dfm-context-id(call),
       failed-generic: f.model-variable-name,
       failed-type-estimates: arg-te*);
    #()
  end;
end method;

define method guaranteed-complete-method-chain 
    (leading-sorted :: <method-sequence>, 
       others :: <method-sequence>, arg-te* :: <argument-sequence>) 
 => (method-chain :: false-or(<method-sequence>))
  block (return)
    collecting (chain)
      for (m in leading-sorted)
        let sig = ^function-signature(m);
        if (arguments-guaranteed-joint?
              (arg-te*, ^signature-required(sig), 
               ^signature-number-required(sig)))
          collect-into(chain, m);
          if (~m.^next?)
            return(collected(chain));
          end if;
        else
          return(#f);
        end;
      finally
	/* TODO: THIS IS BAD BAD BAD
        if (empty?(others))
          // TODO: Might be an error because the last one calls next method?
        end;
        collected(chain);
        */
	#f
      end;
    end;
  end;
end method;

// Return the set of methods potentially applicable to the argument types
// in two sets. The first is a sorted sequence, where each method in that
// sequence is guaranteed to be more specific than the next in that
// sequence, and the last in the sequence is guaranteed to be more
// specific than all the remaining methods. The second set consists
// of the remaining potentially applicable methods that can't be
// usefully ordered.

define method guaranteed-sorted-applicable-methods
  (methods :: <method-sequence>, arg-te* :: <argument-sequence>) 
    => (leading-sorted :: <method-sequence>, others :: <method-sequence>)
  let leading-sorted = #();
  let others = #();
  local method merge-method (m, sorted)
    if (empty?(sorted))
      local method after-m? (other-m)
        guaranteed-method-precedes?(m, other-m, arg-te*);
      end;
      block (return)
        for (other-m in others)
          unless (after-m?(other-m))
            others := pair(m, others);
            return(#());
          end unless;
        end for;
        list(m);
      end block;
      /*
      if (every?(after-m?, others))
        list(m);
      else
        others := pair(m, others);
        #();
      end;
      */
    else
      let lead = sorted.first;
      select (guaranteed-method-relationship(m, lead, arg-te*))
        $method1-precedes
          => pair(m, sorted);
        $method2-precedes
          => pair(lead, merge-method(m, sorted.tail));
        $methods-unordered
          => // Unordered methods are no use to us, so abandon everything
             // from here on.
             others := pair(m, concatenate(sorted, others));
             #();
      end;
    end;
  end;
  for (m in methods)
    let sig = ^function-signature(m);
    if (arguments-potentially-joint?
         (arg-te*, ^signature-required(sig), ^signature-number-required(sig)))
      leading-sorted := merge-method(m, leading-sorted);
    end;
  end;
  values(leading-sorted, others)
end method;

// Determine whether there is some guaranteed ordering between the two
// methods for the given argument type estimates.

define constant $methods-unordered = #"unordered";
define constant $method1-precedes  = #"method1";
define constant $method2-precedes  = #"method2";

define method guaranteed-method-precedes?
    (m1 :: <&method>, m2 :: <&method>, arg-te* :: <argument-sequence>) 
  guaranteed-method-relationship(m1, m2, arg-te*) == $method1-precedes
end method;

// This is pretty much as per the DRM specification, but using the
// conservative "guaranteed-xxx" predicates.

define method guaranteed-method-relationship
    (m1 :: <&method>, m2 :: <&method>, arg-te* :: <argument-sequence>) 
 => (relationship)
  let specs1 = m1.^function-specializers;
  let specs2 = m2.^function-specializers;
  let precedes-somewhere? = #f;
  let follows-somewhere? = #f;
  for (arg-te in arg-te*, spec1 in specs1, spec2 in specs2)
    case
      spec1 == spec2
        => ; // continue
      guaranteed-preceding-specializer?(spec1, spec2, arg-te)
	=> unless (^subtype?(spec2, spec1)) precedes-somewhere? := #t end;
      guaranteed-preceding-specializer?(spec2, spec1, arg-te)
        => follows-somewhere? := #t;
      otherwise
        => ; // continue
    end;
  end;
  if (precedes-somewhere?)
    if (follows-somewhere?)
      $methods-unordered
    else
      $method1-precedes
    end
  else
    if (follows-somewhere?)
      $method2-precedes
    else
      $methods-unordered
    end;
  end;
end method;

// Do we know all the methods on this generic function for the given
// argument type estimates.

define method all-applicable-methods-guaranteed-known? 
    (f :: <&generic-function>, arg-te* :: <argument-sequence>) => (known?)
  if (^generic-function-sealed?(f))
    let binding = model-variable-binding(f);
    let methods = binding-method-definitions(binding);
    ~any?(form-dynamic?, methods);
  else
    let domains = ^generic-function-domains-known(f);
    if (empty?(domains))
      #f
    else
      local method domain-guaranteed-joint? (domain)
        let types = ^domain-types(domain);
        arguments-guaranteed-joint?(arg-te*, types, size(types))
          & all-domain-methods-guaranteed-known?(f, domain, arg-te*)
      end;
      any?(domain-guaranteed-joint?, domains);
    end;
  end;
end method;

define method all-domain-methods-guaranteed-known?
    (f :: <&generic-function>, domain :: <&domain>, arg-te* :: <argument-sequence>)
 => (known? :: <boolean>)
  let ld = model-library(domain);
  let affected-method-definitions
    = if (current-library-description?(ld))
        binding-method-definitions(model-variable-binding(f))
      else
	with-dependent-context ($compilation of model-creator(domain))
          // We don't do cross-library dependency tracking right now.
          without-dependency-tracking
            binding-method-definitions(model-variable-binding(f));
          end;
        end;
      end;
  // format-out(">>> Methods in %= for %=:\n", ld, f);
  /*
  for (md in affected-method-definitions)
    format-out(">>>   %=\n", md);
  end;
  */
  ~any?(form-dynamic?, affected-method-definitions)
end method;

// TODO: This test of domain inclusion is insufficient. If the generic is
// individually sealed over both branches of a test union type, we 
// should win here.

//   domain <integer>
//   domain <symbol>
//   sealed-over type-union(<integer>, <symbol>)?

// Coalescing domain declarations could be tricky to get right. It might
// be easier to expand any type estimate unions here? Unions aren't the
// only things, though. Limited integers have the same problem (although
// they could be considered unions). In fact, there are examples of 
// classes alone.

//   sealed <a> (<object>)
//   <b> (<a>)
//   <c> (<a>)
//   domain (<b>)
//   domain (<c>)
//   sealed-over <a> ?

// Easier if we canonicalize the type a union of known concrete subtypes
// I guess.

define method arguments-guaranteed-joint? 
    (arg-te* :: <argument-sequence>, domain-type* :: <argument-sequence>, max :: <integer>) 
 => (joint?)
  block (return)
    for (arg-te in arg-te*, domain-type in domain-type*,
         i :: <integer> from 0 below max)
      unless (guaranteed-joint?(arg-te, domain-type))
        return(#f)
      end unless;
    end for;
    #t
  end block;
  // every?(guaranteed-joint?, arg-te*, domain-type*);
end method;

define method arguments-potentially-joint? 
    (arg-te* :: <argument-sequence>, domain-type* :: <argument-sequence>, max :: <integer>) 
 => (joint?)
  block (return)
    for (arg-te in arg-te*, domain-type in domain-type*,
         i :: <integer> from 0 below max)
      unless (potentially-joint?(arg-te, domain-type))
        return(#f)
      end unless;
    end for;
    #t
  end block;
  // every?(potentially-joint?, arg-te*, domain-type*);
end method;

// eof
