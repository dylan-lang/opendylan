Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Protocol followed by the typist.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic make
    (type :: <type>, #rest init-args, #key, #all-keys) => (object);

define inline method make
    (type :: <type>, #rest init-args, #key, #all-keys) => (object)
  apply(dylan/make, type, init-args)
end method;

define constant make-type-estimate = make;

///
/// Protocol for typist implementations.
///
/// A type inference algorithm must supply 3 things:
///
/// [1] its theory of types, 
/// [2] operations on those types (algebra and predicates), and 
/// [3] how to infer those types from dfm objects.
///

// You can display a LispWorks graph of these with:
/*
(progn 
(defun browz (&rest names) 
  (tools:make-class-browser 
     :root (mapcar #'(lambda (x) (find-class (intern x 'dylan))) names)))

;(browz "<ENVIRONMENT>")
;(browz "<&RAW-TYPE>")
(browz "<COMPUTATION>")
(browz "<&OBJECT>") ;really <MODEL-VALUE>, nowadays.
(browz "<BINDING>" "<TEMPORARY>"))
*/   
// Input to the typist.
define constant <dfm-ref> = type-union(<computation>, <model-value>, 
                                       <binding>, <value-reference>);

///
/// [1] A theory of types.
///
/// The types are, by and large, the model classes.  However, we have to 
/// annotate them a bit, with justifications and "extra" information that
/// allows us to have a more finely-grained type system than raw Dylan.
///
/// Since we don't want to have multiple copies of the model classes, we
/// use a wrapper which can contain this additional information.  So the 
/// typist operates on <type-estimate>s, which (usually) contain model types.
///

define abstract class <type-estimate> (<object>)
  // Implementations of the typist protocol subclass this.
  slot type-estimate-to-be-normalized? :: <boolean> = #t,
    init-keyword: normalize?:;
end;

define sealed domain make (subclass(<type-estimate>));
define sealed domain initialize (<type-estimate>);

define method make
    (cl :: subclass(<type-estimate>), #key normalize? = #t)
 => (te :: <type-estimate>)
  // :AFTER method: ensure types people make are normalized.  To avoid infinite
  // loops and near-infinite consing, the normalize?: keyword is provided.  
  // Only methods on type-estimate-normalize should ever supply it with #f.
  let estimate = next-method();
  if (normalize?)
    type-estimate-normalize(estimate)
  else
    estimate
  end
end;

define thread variable *printing-a-type-estimate?*    :: <boolean> = #f;
define generic print-type-estimate-internals(te :: <type-estimate>, #key stream)
  => ();

define method print-object(te :: <type-estimate>, stream :: <stream>) => ()
  // :AROUND method: print wrapper for outermost <type-estimate> only.
  if (*printing-a-type-estimate?*)
    print-type-estimate-internals(te, stream: stream)
  else
    dynamic-bind (*printing-a-type-estimate?* = #t)
      write(stream, "{Type Estimate: ");
      print-type-estimate-internals(te, stream: stream);
      write-element(stream, '}')
    end
  end
end;

/*
define method print-message(te :: <type-estimate>, stream :: <stream>) => ()
  dynamic-bind (*printing-a-type-estimate?* = #t)
    print-type-estimate-internals(te, stream: stream);
  end
end;
*/


///
/// [2] The algebra of types.
///

define generic type-estimate-normalize(te1 :: <type-estimate>)
  => (te :: <type-estimate>);

define generic type-estimate-union(te1 :: <type-estimate>, 
                                   te2 :: <type-estimate>)
  => (te :: <type-estimate>);

define generic type-estimate-intersection(te1 :: <type-estimate>, 
                                          te2 :: <type-estimate>)
  => (te :: <type-estimate>);

define generic type-estimate-difference(te1 :: <type-estimate>,
                                        te2 :: <type-estimate>)
  => (te :: <type-estimate>);

// *** type-estimate-not(te)?

define generic type-estimate-base(te :: <type-estimate>) 
  => (te-base :: <type-estimate>);

define generic type-estimate-match?(te1 /* :: <type-estimate> */, 
                                    te2 /* :: <type-estimate> */) 
  => (match? :: <boolean>);

///
/// Predicates on types: 
///

define generic type-estimate-instance?(ref :: <dfm-ref>, te :: <type-estimate>)
  => (instance? :: <boolean>, known? :: <boolean>);

define generic type-estimate-disjoint?(te1 :: <type-estimate>, 
                                       te2 :: <type-estimate>)
  => (disjoint? :: <boolean>, known? :: <boolean>);

define generic type-estimate-subtype?(te1 :: <type-estimate>, 
                                      te2 :: <type-estimate>)
  => (subtype? :: <boolean>, known? :: <boolean>);

define function type-estimate=?(te1 :: <type-estimate>, te2 :: <type-estimate>)
  => (e? :: <boolean>, known? :: <boolean>)
  // Dylan Torah, p. 48: te1 = te2 iff te1 <= te2 & te2 <= te1
  let (sub?-1, known?-1) = type-estimate-subtype?(te1, te2);
  let (sub?-2, known?-2) = type-estimate-subtype?(te1, te2);
  if (known?-1 & known?-2)                               // Both answers known
    values(sub?-1 & sub?-2, #t) 
  elseif ((known?-1 & ~ sub?-1) | (known?-2 & ~ sub?-2)) // At least 1 known=#f
    values(#f, #t)
  else                                                   // Little known; punt
    values(#f, #f)
  end
end;

define function type-estimate-pseudosubtype?(te1 :: <type-estimate>, 
                                             te2 :: <type-estimate>)
  => (pseudosubtype? :: <boolean>, known? :: <boolean>)
  // Dylan Torah, p. 48: te1 pseudo<= te2 iff t1 <= base(t2) & ~(t1 disjoint t2)
  let (sub?,  know-sub?) = type-estimate-subtype?(te1, type-estimate-base(te2));
  let (disj?, know-dis?) = type-estimate-disjoint?(te1, te2);
  if (know-sub? & know-dis?)                          // Both answers known
    values(sub? & ~disj?, #t)
  elseif ((know-sub? & ~ sub?) | (know-dis? & disj?)) // At least 1 known to fail
    values(#f, #t)
  else                                                // Not enough known
    values(#f, #f)
  end
end;

///
/// [3] How to infer the types.
///
/// * initialize-typist-library-caches (ld :: <library-description>) => ()
///  
///   Write an empty <type-cache> and disjoint? cache in the library
///   description.  Used when a library description is made, and when you
///   want to clear out the type info for taking another pass after
///   optimizations.
///
/// * type-estimate-in-cache(ref :: <dfm-ref>, cache :: <type-cache>) 
///      => (te :: <type-estimate>)
///
///   Gets the compile-time type estimate of ref, possibly looking in cache, 
///   updating cache as necessary.  Inference, when done, is done by calling 
///   type-estimate-infer, below, with the dfm-ref being typed, the whole cache,
///   and what's in the cache under the dfm-ref (to be updated).
///
/// * type-estimate-infer(ref :: <dfm-ref>, cache :: <type-cache>)
///      => (te :: <type-estimate>)
///
///   Computes the compile-time type estimate in the first place, updating
///   result in cache (unioning with what's already there).  Implementors supply
///   methods for type-estimate-infer.  You never want to call this; go through
///   type-estimate instead to get the benefit of caching.  Not exported.
///
/// * type-estimate-retract(ref :: <dfm-ref>, cache :: <type-cache>)
///      => (did-anything? :: <boolean>)
///
///   Pulls this type estimate out and all other which depend only upon it.
///   What you want for incremental retyping, e.g., during optimizations.
///   Return value is true if anything happened, #f if nothing needed be done.
///
/// * type-estimate-explain(ref   :: <dfm-ref>,
///                         cache :: <type-cache>,
///                         #key stream   :: <stream> = *standard-output*, 
///                              recurse? :: <boolean> = #f,
///                              indent   :: <integer> = 0)
///      => ()
///
///   Prints an explanation of the type-estimate of ref to stream.
///

// define constant <justifications> = limited(<sequence>, of: <justification>);
define constant <justifications> = <list>;

define class <type-variable> (<object>)
  // These are the things that go in the cache and in types that have other
  // types as components (functions, multiple values, etc.).  They contain the 
  // type and the justification links.  Boxes for type estimates+add'l info.
  slot type-variable-contents :: <type-estimate> = make(<type-estimate-bottom>),
    init-keyword: contents:;
  // Rule firings that have flowed types into this variable.
  slot type-variable-supporters :: <justifications> = #(),
    init-keyword: supporters:;
  // Rule firings for which types flow out of this variable to somewhere else.
  slot type-variable-supportees :: <justifications> = #(),
    init-keyword: supportees:;
end;

define sealed domain make (subclass(<type-variable>));
define sealed domain initialize (<type-variable>);

define method print-object(v :: <type-variable>, stream :: <stream>) => ()
  // How to print a <type-variable>.
  format(stream, "{Type-Variable: %=}", type-variable-contents(v))
end;

define constant <type-cache>     
  = limited(<table>, of: false-or(<type-variable>));
define constant <type-variables> 
  = // limited(<sequence>, of: <type-variable>);
    <list>;
define constant <rule-rhs>
  = // limited(<sequence>, // Basis vs induction rules
    //         of: type-union(<dfm-ref>, <type-variable>));
    <list>;

// These are the initial sizes of the caches.  They come from compiling the
// Dylan library and inspecting the resulting entrails.  Jan. 2 '97 data:
//  Size of type cache     = 33652
//  Size of disjoint cache =  1169
//  Size of cons cache     =   129
// define constant $type-cache-size-init$     :: <integer> = 35000;
// define constant $disjoint-cache-size-init$ :: <integer> =  2000;
// define constant $cons-cache-size-init$     :: <integer> =   200;

define compiler-sideways method initialize-typist-library-caches 
    (ld :: <compilation-context>) => ()
  // When you make a <library-description> or an interactive layer, install
  // typist caches.   Done this way because of module lossage, i.e. typist
  // classes aren't visible
  // in dfmc-namespace, so couldn't do it with slot initializers. Sigh.
  library-type-cache(ld) := 
    make(<type-cache> /* , size: $type-cache-size-init$ */);
  library-type-estimate-disjoint?-cache(ld) := 
    make(<type-estimate-pair-match-table> /* , size: $disjoint-cache-size-init$ */);
  library-type-estimate-cons-cache(ld) := 
    make(<table> /* , size: $cons-cache-size-init$ */);
  library-type-estimate-dispatch-cache(ld) := 
    make(<table> /* , size: $cons-cache-size-init$ */);
end;

// Current justification context.  Bound per thread by rule code.
define thread variable *current-rule* :: false-or(<symbol>)   = #f;  // Type rule
define thread variable *current-lhs*  :: false-or(<dfm-ref>)  = #f;  // Trigger
define thread variable *current-rhs*  :: false-or(<rule-rhs>) = #(); // Action(s)

define class <justification> (<object>)
  // How a type estimate remembers its supporters & supportees (bidirectional).
  // A justification represents a firing of a type inference rule.
  constant slot justification-rule :: <symbol>   = *current-rule*,
    init-keyword: rule:;
  constant slot justification-lhs  :: <dfm-ref>  = *current-lhs*,
    init-keyword: lhs:;
  constant slot justification-rhs  :: <rule-rhs> = *current-rhs*,
    init-keyword: rhs:;
end;

define method print-object(j :: <justification>, stream :: <stream>) => ()
  // How to print a justification.
  format(stream, "{Rule: %=, LHS: %=", 
         justification-rule(j), justification-lhs(j));
  when (justification-rhs(j))
    format(stream, ", RHS: ");
    print-separated-collection(justification-rhs(j),
                               stream: stream,
                               conjunction: "and")
  end;
  format(stream, "}")
end;

define generic type-estimate-in-cache(ref :: <dfm-ref>, cache :: <type-cache>)
  => (te :: <type-estimate>);

define generic type-estimate(ref :: <dfm-ref>)
 => (te :: <type-estimate>);

define generic type-estimate-infer(ref :: <dfm-ref>, cache :: <type-cache>)
  => (te :: <type-estimate>);

// lookup-type is now the main interface to the typist (replacing type-estimate)  
define generic lookup-type (object :: <dfm-ref>, 
			    css :: <call-site-summary>, 
			    computation :: <computation>)
  => (result :: <type-estimate>);

define method lookup-type 
  (ref :: <dfm-ref>, css :: <call-site-summary>, computation :: <computation>)
  => (result :: <type-estimate>)
  type-estimate(ref)
end;

// for new typist api -- gts
define method type-initializer-method(l :: <&lambda>)
  type-estimate(l)
end method;

define thread variable *type-vars-propagating* :: <list> = #();  // See below!

// *** For future reference: adding a type causes propagation, as done here.
//     But propagation can find more methods, which adds more types, too.
//     Cf. Agesen, p. 39.
define method type-estimate-update-cache(ref      :: <dfm-ref>, 
					 cache    :: <type-cache>, 
					 new-type :: <type-estimate>) => ()
  let ref = canonical-ref(ref);
  // Subroutine to update the cached type-estimate &c w/newly inferred result.
  // Usefully called by methods on type-estimate-infer, generated by rules.
  let ref-variable = cached-type-variable(ref, cache);
  debug-assert(ref-variable ~== #f, "Can't find type variable for ref %s.", ref);
  unless (member?(ref-variable, *type-vars-propagating*))
    dynamic-bind (*type-vars-propagating* 
		    = pair(ref-variable, *type-vars-propagating*))
      // Don't recurse infinitely in the face of dependency graph cycles.
      type-variable-contents(ref-variable) :=
        type-estimate-union(type-variable-contents(ref-variable), new-type);
      // Justifications are bidirectional links.
      let just = make(<justification>);           // Defaults from per thread binding
      type-variable-supporters(ref-variable) := 
        add!(type-variable-supporters(ref-variable), just);
      for (next-rhs in *current-rhs*)             // Each RHS supports us
        unless (instance?(next-rhs, <type-variable>))
          next-rhs := cached-type-variable(canonical-ref(next-rhs), cache);
        end unless;
        type-variable-supportees(next-rhs) := 
          add!(type-variable-supportees(next-rhs), just)
      end;
      // Now make it propagate, to the supportees.  Depth-first, if you care.
      // Remember that this type variable is already being propagated.
      for (supportee in type-variable-supportees(ref-variable))
        type-estimate-update-cache(justification-lhs(supportee), cache, new-type)
      end
    end
  end;
  values()
end;

define generic canonical-ref (ref) => (ref);

define method canonical-ref (ref :: <dfm-ref>) => (ref :: <dfm-ref>)
  ref
end method;

define method canonical-ref (ref :: <temporary>)
    => (ref :: type-union(<computation>, <temporary>));
  // Remember that <temporary>s aren't in the type cache any more.
  generator(ref) | ref
end method;

// Get around emulator bug in commented-out select dispatch above.
define method canonical-ref (ref :: <object-reference>) => (ref)
  canonical-ref(reference-value(ref))
end method;

define method canonical-ref
    (ref :: <defined-constant-reference>) => (ref)
  canonical-ref(referenced-binding(ref))
end method;

///
/// The main entry point to the typist is type-estimate-in-cache.  It's only
/// generic to separate out the case of <temporary>.
///

define inline method type-estimate (ref :: <dfm-ref>) => (te :: <type-estimate>)
  type-estimate-in-cache(ref, library-type-cache(current-library-description()))
end method;

define method type-estimate-in-cache (ref :: <dfm-ref>, cache :: <type-cache>) 
    => (te :: <type-estimate>)
  let ref = canonical-ref(ref);
  // Look in cache or infer a type.  Prefer cache if nonempty.
  // Cache may "contain" #f (this ref hasn't been seen yet), bottom (this ref 
  // either has no type, or a type computation is pending), or some other type
  // (type inference has already given a preliminary answer).
  let type-var = cached-type-variable(ref, cache);
  if (type-var)
    // Go with what's in the cache
    type-variable-contents(type-var)
  else
    // Fill (empty) cache with bottom and go infer.
    // Break infinite recursion w/bottom
    cached-type-variable(ref, cache) := make(<type-variable>); 
    type-estimate-infer(ref, cache)      // Infer the type
  end
end;

define method cached-type-variable
    (ref :: <computation>, cache :: <type-cache>)
 => (te :: false-or(<type-variable>))
  computation-type(ref)
end method;

define method cached-type-variable-setter
    (tv :: <type-variable>, ref :: <computation>, cache :: <type-cache>)
 => (tv :: <type-variable>)
  computation-type(ref) := tv;
end method;

define method retract-cached-type-variable
    (ref :: <computation>, cache :: <type-cache>) => ()
  computation-type(ref) := #f
end method;

define method cached-type-variable
    (ref :: <dfm-ref>, cache :: <type-cache>)
 => (tv :: false-or(<type-variable>))
  element(cache, ref, default: #f);
end method;

define method cached-type-variable-setter
    (tv :: <type-variable>, ref :: <dfm-ref>, cache :: <type-cache>)
 => (tv :: <type-variable>)
  element(cache, ref) := tv;
end method;

define method retract-cached-type-variable
    (ref :: <object>, cache :: <type-cache>) => ()
  remove-key!(cache, ref);
end method;

///
/// Figuring out how you got here.
///

define generic type-estimate-explain(ref :: <dfm-ref>, cache :: <type-cache>, 
				     #key stream   :: <stream>  = *standard-output*, 
				          recurse? :: <boolean> = #f, 
				          indent   :: <integer> = 0)
    => ();

define method type-estimate-explain
   (ref :: <temporary>, cache :: <type-cache>, 
    #rest keys,
    #key stream   :: <stream> = *standard-output*, 
         recurse? :: <boolean> = #f,
         indent   :: <integer> = 0) => ()
  // Special case for temporaries is to punt to their generators.
  let g = generator(ref);
  if (g)
    apply(next-method, g, cache, keys);
  else
    // Lambda variables have no generators, but are in the cache.
    next-method();
  end;
end;

define method type-estimate-explain
   (ref :: <dfm-ref>, cache :: <type-cache>, 
    #key stream   :: <stream> = *standard-output*, 
         recurse? :: <boolean> = #f,
         indent   :: <integer> = 0)
   => ()
 // Explain this type-estimate.
 local method indentify (#key levels = 0)
         // A "level" is 2 spaces.  indent: gives leftmost margin.  Levels is
         // add'l number of levels beyond that.  Oh, for with-indentation.
         format(stream, "\n");
         for (level from 0 below indent + levels)
           write(stream, "  ")
         end
       end;
 let ref-type = type-estimate-in-cache(ref, cache);
 indentify(levels: 0);
 format(stream, "Object %= (a %=) :: %=", ref, object-class(ref), ref-type);
 indentify(levels: 0); format(stream, "because: ");
 print-separated-collection(
     type-variable-supporters(cached-type-variable(ref, cache)),
     stream:  stream,
     printer: method(just :: <justification>, stream)
                indentify(levels: 1); 
                format(stream, "Rule %=", justification-rule(just));
                unless (empty?(justification-rhs(just)))
                  format(stream, " on ");
                  print-separated-collection(justification-rhs(just), 
                                             stream: stream, conjunction: "and")
                end;
                when (recurse? & ~empty?(justification-rhs(just)))
                  // Wants to recurse & not at bottom.  Explain each
                  // of the elements of the RHS.
                  do(rcurry(type-estimate-explain,
                            cache, stream: stream, recurse?: #t, 
                            indent: indent + 1), 
                     justification-rhs(just))
                end
              end);
 values()
end;

define generic type-estimate-retract(ref :: <dfm-ref>)
  => (did-anything? :: <boolean>);

///
/// Dispatch caching.
///

define method type-estimate-dispatch-cache-lookup
    (gf :: <&generic-function>, te* :: <object>) => (methods)
  let dispatch-cache
    = library-type-estimate-dispatch-cache(current-library-description());
  let gf-cache 
    = element(dispatch-cache, gf, default: #f);
  if (gf-cache)
    let methods = element(gf-cache, te*, default: #f);
    if (methods)
      // format-out(">>> Hit %=%=: %=\n", gf, te*, methods);
      // format-out(">>> Hit %s\n", ^debug-name(gf));
    end;
    methods
  end;
end method;

define method add-type-estimate-dispatch-cache-entry
    (gf :: <&generic-function>, te* :: <object>, methods) => ()
  // format-out(">>> Caching %s%s: %=\n", ^debug-name(gf), te*, methods);
  // format-out(">>> Caching %s\n", ^debug-name(gf));
  let dispatch-cache
    = library-type-estimate-dispatch-cache(current-library-description());
  let gf-cache 
    = element(dispatch-cache, gf, default: #f) 
        | (element(dispatch-cache, gf)
             := make(<type-estimate-sequence-match-table>));
  element(gf-cache, te*) := methods;
end method;
