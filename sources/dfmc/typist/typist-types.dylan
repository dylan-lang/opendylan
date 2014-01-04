Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Theory of the types in the typist.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// Kinds of Dylan types ("kind" used as in FX-91):
///
/// * Top 
///
///   - Common supertype of "normal" stuff (under <object>) and raw types.
///
/// * Classes
///
///   - Almost everything "normal" is one of these.
/// 
/// * Raw types
///
///   - Low-level stuff, under <&raw-type>.
///
/// * Limiteds -- built on classes, i.e., the class being limited.  
///
///   - Limited Integer: min (default -infinity) & max (default +infinity).
///
///   - Limited Class: subclass(x) denotes any class which is a subclass of x.
///
///   - Limited Instance: singleton(x) denotes anything == to x.
///
///   - Limited Collection: some collections support of: to limit the type of 
///     their elements, default-fill: to provide a valid default fill value for
///     size-setter or make, size: to limit their overall size, and dimensions:
///     if they happen to be arrays.
///
///     More details:
///       <collection>, <explicit-key-collection>, <mutable-collection>,
///       <stretchy-collection>, <mutable-explicit-key-collection>, <sequence>,
///       <mutable-sequence> support of:, size: and return uninstantiable types.
///
///       <table>, <object-table> support of:, size: and return an instantiable
///          type that supports a size: initialization.
///
///       <array> supports of:, default-fill:, size:, dimensions: and returns an
///          instantiable type that supports dimensions: and fill:
///          initializations.  Note constraint between fill: and of:.
///
///       <vector>, <simple-vector> support of:, default-fill:, size: and
///          return an instantiable type which takes size: and fill: keys. Note
///          constraint between fill: and of:.
///
///       <stretchy-vector>, <deque> support of: and default-fill: and return an 
///          instantiable type which takes size: and fill: keys.  Note 
///          constraint between fill: and of:.
///
///       <string> supports of:, default-fill:, size: and returns an
///          instantiable type supporting size: and fill:.  of: must be subtype
///          of character.
///
///       <range> supports of: (subtype of <real>).  Result takes from:, to:, 
///          below:, above:, by:, size:.
///
///   - Limited Function: limits <function> to certain arg & value types.
///
/// * Union: the result of a type-union in user code, or the result of static
///   imprecision in type inference.
///
/// * Multiple Values: We're really typing DFM code, not Dylan code.  DFM code
///   creates boxes to return multiple values (even 0 or 1 value).  While 
///   optimizations should remove most of that, we still have to deal with it.
///
/// * Bottom: bottom of the type lattice.  Never has instances.  (Like NIL as 
///   a type in Common Lisp.)  Inferred only in case of an error, or unreachable
///   code.  Can result from promiscuous intersection or difference operations.
///
/// You can display a LispWorks graph of these with:
/// (tools:make-class-browser :root (list (find-class 'dylan::<type-estimate>)))
///

define constant <unionee-sequence> = <list>;
define constant <fixed-sequence>   = <simple-object-vector>;
define constant <type-variable-vector> = <simple-object-vector>;

define generic type-estimate-debug-name (x) => (dn :: <string>);

define method type-estimate-debug-name(o :: <object>) => (dn :: <string>)
  // Last-ditch attempt: just print it to a string.
  let str = make(<byte-string-stream>, direction: #"output");
  format(str, "%s", o);
  stream-contents(str)
end;

define method type-estimate-debug-name(o :: <&object>) => (dn :: <string>)
  // Attempt to extract the debug-name.
  let debug-str = ^debug-name(o);
  if (debug-str)
    as(<string>, debug-str)
  else
    // format-out("\n *** type-estimate-debug-name(%s) punted.", o);
    next-method()
  end
end;

///
/// Macro for defining <type-estimate> classes.
///
/// OK, so why this macro?  
///
/// Some type estimates contain other type estimates (e.g., multiple-values,
/// limited-functions, etc.).  Make them contain a <type-variable> instead, 
/// so constraints can be propagated properly.  What this macro does
/// is arrange for the "usual" accessors to get you the _contents_ of the
/// <type-variable>, transparently.
///
/// Implementation: type-class is like class & type-slot is like slot.
///
/// define type-class <foo> (<quux>)
///   slot slot1 :: <integer> = 5, init-keyword: 1:;
///   type-slot slot2-variable = make(<type-variable>, ...), init-keyword: 2:;
/// end;
///
/// * Defines the class <foo>.
/// * Ordinary slots are ordinary slots of <foo>.
/// * For each type-slot, e.g., slot2-variable, define 2 real + 2 virtual slots:
///   - SLOT2-VARIABLE-INTERNAL is a real slot to hold the type variable, or 
///     structure containing type variables.  This gets all the slot options;
///     an initialize method makes sure the init values are coerced to 
///     type variables or structures thereof.
///   - SLOT2-CACHE is a real slot to hold the value of the type variable, or 
///     structure copy containing the type variable values.  When not valid, 
///     it contains a distinguished "$unfound" value (common-extensions).
///   - SLOT2 is a constant virtual slot.  On reads, it reads slot2-cache and
///     refreshes it if necessary by copying & dereferencing from 
///     slot2-variable-internal.  It is not writeable.
///   - SLOT2-VARIABLE is a virtual slot.  On reads, it just reads 
///     slot2-variable-internal.  On writes, it writes slot2-variable-internal 
///     and invalidates slot2-cache.
///
/// (Hairy because of lack of daemon combination methods.  Both virtual slots
///  could be daemons on accessors for the real slots.)
///

define macro type-class-definer
  // Copy slots so one can generate real slots, the other accessor methods.
  { define ?cadjs:* type-class ?cname:name (?supers:*) ?slots:* end }
    => { define ?cadjs type-class-aux ?cname (?supers) (?slots) (?slots) end }
end;

define macro type-class-aux-definer // Ugly.  Ugly bag of mostly water.
  // Generate class, slots, & accessors.
  { define ?cadjs:* type-class-aux ?cname:name (?supers:*)
      (?slots) (?methods)
    end }
    => { define ?cadjs class ?cname (?supers) ?slots end;// Class defn
         define type-class-accessors ?cname (?methods) } // Virtual slot mthds
slots:
  { }               => { }                               // ;-separated sequence
  { ?aslot:*; ... } => { ?aslot; ... }                   // of slot specs
aslot:
  { type-slot ?sname:name ## "-VARIABLE" ?stuff:* }      // type slot
    => { constant slot ?sname ## "-VARIABLE-INTERNAL" ?stuff;     // where type var lives
         slot ?sname ## "-CACHE" = $unfound }         //  cache for estimate
  { ?sadjs:* slot ?stuff:* } => { ?sadjs slot ?stuff }   // ordinary slot
  { keyword ?stuff:* }       => { keyword ?stuff }       // keyword decl
methods:
  { }               => { }                               // ;-separated sequence
  { ?amethod; ... } => { ?amethod, ... }                 // of method specs
amethod:                                                 // Spec/ordin accessors
  { type-slot ?sname:name ## "-VARIABLE" ?stuff:* } => { ?sname }
  { ?ordinary:* }                                   => { #f }
end;

define macro type-class-accessors-definer
  { define type-class-accessors ?cname:name () }
    => { }
  { define type-class-accessors ?cname:name (#f, ?rest:*) }
    => { define type-class-accessors ?cname (?rest) }
  { define type-class-accessors ?cname:name (?sname:name, ?rest:*) }
    => { /*
         define inline method ?sname ## "-VARIABLE" (x :: ?cname)
           ?sname ## "-VARIABLE-INTERNAL"(x)              // Reader is simple
         end;
         define inline method ?sname ## "-VARIABLE-SETTER"
             (x :: ?cname, new-value)
           ?sname ## "-CACHE"(x) := $unfound;          // Writer invalidates
           ?sname ## "-VARIABLE-INTERNAL"(x) := new-value //   the cache
         end;
         */
         define method ?sname (x :: ?cname)               // Reader only
           let cache-value = ?sname ## "-CACHE" (x);      // Value from the cache
           if (found?(cache-value))                       // Use cache if valid
             cache-value
           else
             ?sname ## "-CACHE"(x) :=                     // else refresh
               make-cache-copy(?sname ## "-VARIABLE-INTERNAL"(x))
           end
         end;
         define type-class-accessors ?cname (?rest) }
end;

// Look at what's stored in name-VARIABLE-INTERNAL, and return what to 
// write into name-CACHE.  Methods are based on a case analysis of what 
// can occur in the type-slot of types below.
define generic make-cache-copy (x :: <object>) => (deref-copy :: <object>);

define method make-cache-copy(x :: <object>) => (copy)
  // Default method: just return it.  All other methods recurse until here.
  x
end;

define method make-cache-copy(x :: <type-variable>) => (copy)
  // Just dereference the type variable
  make-cache-copy(type-variable-contents(x))
end;

define method make-cache-copy(x :: <collection>) => (copy)
  // Map over collection, dereferenceing
  map(make-cache-copy, x)
end;

///
/// Some coercion routines that would, in a kinder, gentler world, be AS methods.
///

// *** Would be as(limited(<collection>, of: <type-variable>), <sequence>) method,
//     but limited doesn't really work in the emulator!
define inline function as-false-or-type-variable-coll(x :: false-or(<collection>))
    => (result :: false-or(<collection>))
  // Coerce a collection to a collection of type variables.
  if (x == #f |                                          // unsupplied keyword
        every?(rcurry(instance?, <type-variable>), x))   // proper collection
    x
  else
    map(curry(make, <type-variable>, contents:), x)
  end
end;

// *** Would be as(false-or(<type-variable>), <object>), but...
define function as-false-or-type-variable(x) 
    => (result :: false-or(<type-variable>))
  // Coerce to #f or a type variable.
  if (x == #f | instance?(x, <type-variable>))
    x
  else
    make(<type-variable>, contents: x)
  end
end;

///
/// Top <type-estimate>.
///

define type-class <type-estimate-top> (<type-estimate>)
end;

define constant $top-cons-cache-key$ :: <pair> = list(#"top");

define method make (cl == <type-estimate-top>, #rest keys, #key, #all-keys) 
 => (te :: <type-estimate-top>)
  // Use the canonical one in the cons cache, if it's there.
  // Note that since there's no model-object to take model-library of, 
  // we pretty much have to do current-library-description().
  let te-cons-cache = library-type-estimate-cons-cache(current-library-description());
  let cache-val     = element(te-cons-cache, $top-cons-cache-key$, 
                              default: #f);
  if (cache-val)
    cache-val
  else
    let the-top = next-method();
    te-cons-cache[$top-cons-cache-key$] := the-top;
    the-top
  end
end;

define method print-type-estimate-internals (tte :: <type-estimate-top>, 
                                             #key stream) => ()
  format(stream, "<top>")
end;

///
/// Class <type-estimate>s.
/// 

// TODO: Tighten this constraint back to class. Currently, limited instances
// of raw types get made which inherit this slot but fill it with a raw
// type.

define type-class <type-estimate-class> (<type-estimate>)
  constant slot type-estimate-class /* :: <&class> */,
    required-init-keyword: class:;
end;

define method make (cl == <type-estimate-class>, #key class, #all-keys)
 => (te :: <type-estimate-class>)
  // Look it up in the cache first, if you can.  Note the specializer on cl
  // is a SINGLETON, not subclass.  We certainly don't want to inherit this!
  let te-cons-cache = library-type-estimate-cons-cache(model-library(class));
  let cache-val     = element(te-cons-cache, class, default: #f);
  if (cache-val)
    // Found it in the cons cache, so go with that.
    cache-val
  else
    // Cons cache miss, so make one, cache it, and return it.
    let new-te = next-method();
    te-cons-cache[class] := new-te;
    new-te
  end
end;

define method print-type-estimate-internals (cte :: <type-estimate-class>, 
                                             #key stream) => ()
  format(stream, "%s", type-estimate-debug-name(type-estimate-class(cte)))
end;

///
/// Raw <type-estimate>s.
///

define type-class <type-estimate-raw> (<type-estimate>)
  constant slot type-estimate-raw 
      :: type-union(<&raw-type>, <&raw-aggregate-type>), 
    required-init-keyword: raw:;
end;

define method make (cl == <type-estimate-raw>, #key raw, #all-keys)
 => (te :: <type-estimate-raw>)
  // Look it up in the cache first, if you can.  Note the specializer on cl
  // is a SINGLETON, not subclass.  We certainly don't want to inherit this!
  let te-cons-cache = library-type-estimate-cons-cache(model-library(raw));
  let cache-val     = element(te-cons-cache, raw, default: #f);
  if (cache-val)
    // Found it in the cons cache, so go with that.
    cache-val
  else
    // Cons cache miss, so make one, cache it, and return it.
    let new-te = next-method();
    te-cons-cache[raw] := new-te;
    new-te
  end
end;

define method print-type-estimate-internals (raw :: <type-estimate-raw>, 
                                             #key stream) => ()
  format(stream, "%s", type-estimate-debug-name(type-estimate-raw(raw)))
end;

///
/// Multiple values <type-estimate>s.
///

define type-class <type-estimate-values> (<type-estimate>)
  type-slot type-estimate-fixed-values-variable :: <type-variable-vector> = #[], 
    init-keyword: fixed:;
  type-slot type-estimate-rest-values-variable :: false-or(<type-variable>)
    = make(<type-variable>, 
           contents: make(<type-estimate-class>, 
                          class: dylan-value(#"<object>"))),
    init-keyword: rest:;
end;

define method make
    (tev :: subclass(<type-estimate-values>), #rest keys, 
     #key fixed, rest = #"unsupplied", #all-keys)
 => (te :: <type-estimate>)
  // Coerce keywords to type variables and collections thereof.
  let fixed :: false-or(<fixed-sequence>)
    = fixed & as(<fixed-sequence>, fixed);
  apply(next-method, tev, 
        fixed: case
                 fixed     => as-false-or-type-variable-coll(fixed);
                 otherwise => #[];
               end,
        rest:  as-false-or-type-variable(
                 case
                   // Unsupplied means values(#rest <object>).
                   // #f means no rest return type.
                   // A type means that return type.
                   rest == #"unsupplied" => as(<type-estimate>, 
                                               dylan-value(#"<object>"));
                   otherwise             => rest;
                 end), 
        keys)
end;

define method print-type-estimate-internals(vte :: <type-estimate-values>, 
                                            #key stream) => ()
  let fixed-vals = type-estimate-fixed-values(vte);
  let rest-vals  = type-estimate-rest-values(vte);
  write(stream, "values(");
  print-separated-collection(fixed-vals, stream: stream);
  when (rest-vals)
    when (fixed-vals ~== #())
      write(stream, ", ");
    end;
    format(stream, "#rest %s", rest-vals)
  end;
  write-element(stream, ')')
end;

define function type-estimate-values-ref 
  (vte :: <type-estimate-values>, i ::<integer>)
  => (tei :: <type-estimate>, source :: one-of(#"fixed", #"rest", #"default"))
  // Return the ith type in a values type-estimate, and where it 
  // came from: #"fixed", #"rest", or #"default".
  if (i < 0)
    error("Can't take negative (%dth) value from %s!", i, vte)
  elseif (i < size(type-estimate-fixed-values(vte)))
    values(type-estimate-fixed-values(vte)[i], #"fixed")   // In range
  elseif (type-estimate-rest-values(vte))
    values(type-estimate-rest-values(vte), #"rest")        // Out of range, #rest
  else
    values(make(<type-estimate-limited-instance>, singleton: #f), #"default")
  end
end;

///
/// Limited <type-estimate>s.
/// 
/// *** Deal with limited ranges (from: to: below: above: by: and size:) someday.
/// *** Limited strings, and all the specific classes which have limited methods.
///

// *** Should this be a mixin?
define /* abstract */ type-class <type-estimate-limited> (<type-estimate-class>)
  // class slot is the class being limited.
end;

define method initialize(lim :: <type-estimate-limited>, #key class, root)
  next-method();
  when (root)
    // Enforce that the class being limited is a subclass of root.
    // This ensures, e.g., that limited integers are built out of <integer>s,
    // or something like them.  (Obligatory Firesign Theater reference.)
    assert(^subtype?(class, root), 
           "In %=, class: %= being limited is not a subclass of %=.",
           lim, class, root)
  end
end;

///
/// Limited integer <type-estimate>s.
///

define type-class <type-estimate-limited-integer> (<type-estimate-limited>)
  keyword class: = dylan-value(#"<integer>"); 
  keyword root:  = dylan-value(#"<integer>");
  // #f means -infty for min, +infty for max.
  constant slot type-estimate-min :: false-or(<integer>) = #f, 
    init-keyword: min:;
  constant slot type-estimate-max :: false-or(<integer>) = #f,
    init-keyword: max:;
end;

define method initialize(lint :: <type-estimate-limited-integer>, #key min, max)
  next-method();
  assert(min | max, "In %=, expected at least one of MIN: or MAX:.", lint);
  when (min & max)
    // Could just normalize to bottom if min: > max:, I suppose.
    assert(min <= max, "In %=, MIN:(%=) > MAX:(%=).", lint, min, max)
  end
end;

define method print-type-estimate-internals
    (teli :: <type-estimate-limited-integer>, #key stream) => ()
  // E.g., <integer>[0, 10], <integer>[-inf, 10], <integer>[0, +inf].
  format(stream, "limited(%s, min: %s, max: %s)", 
         type-estimate-debug-name(type-estimate-class(teli)),
         type-estimate-min(teli) | "-inf", 
         type-estimate-max(teli) | "+inf")
end;

///
/// Limited class (subclass) <type-estimate>s.
///

define type-class <type-estimate-limited-class> (<type-estimate-limited>)
  keyword class: = dylan-value(#"<class>");
  keyword root:  = dylan-value(#"<class>");
  constant slot type-estimate-subclass :: <&class>, 
    required-init-keyword: subclass:;
end;

// Bad idea to attempt to use the cons cache.  If you key on subclass, you'll 
// conflict with the <type-estimate-class>.  If you make a model subclass,
// you'll have to cons even if the cache hits.

define method initialize(lc :: <type-estimate-limited-class>, #key class, subclass)
  next-method();
  // Tighter type: want subclass to be an instance of whatever's in class.
  assert(^instance?(subclass, class),
         "In %=, subclass %= is not an instance of class type %=.",
         lc, subclass, class)
end;

define method print-type-estimate-internals
    (telc :: <type-estimate-limited-class>, #key stream) => ()
  format(stream, "subclass(%s)", type-estimate-debug-name(type-estimate-subclass(telc)))
end;

///
/// Limited instance (singleton) <type-estimate>s.
///

define type-class <type-estimate-limited-instance> (<type-estimate-limited>)
  constant slot type-estimate-singleton :: <model-value>,
    required-init-keyword: singleton:;
end;

/*
// *** Performance analysis code.
define variable *limited-instances* = #();

// *** Performance analysis code.
define function show-singleton-stats(#key data = *limited-instances*) => ()
  // Investigate the singletons.
  let do-with-testing-context = access(dfmc-testing, do-with-testing-context);
  with-testing-context (#f)
    let n-classes = 0;
    let n-strings = 0;
    let n-numbers = 0;
    let n-funcs   = 0;
    let n-bools   = 0;
    let n-other   = 0;
    for (item in data)
      select (item by instance?)
        <&class>, <class>  => n-classes := n-classes + 1;
        <string>           => n-strings := n-strings + 1;
        <number>           => n-numbers := n-numbers + 1;
        <&callable-object> => n-funcs   := n-funcs   + 1;
        <boolean>          => n-bools   := n-bools   + 1;
        otherwise          => n-other   := n-other   + 1;
      end
    finally
     // Make an alist, so we can sort it.
     let alist = list(pair("Classes:   ", n-classes),
                      pair("Strings:   ", n-strings),
                      pair("Numbers:   ", n-numbers),
                      pair("Functions: ", n-funcs),
                      pair("Booleans:  ", n-bools),
                      pair("Other:     ", n-other));
     alist := sort!(alist, test: method (x, y) rest(x) > rest(y) end);
     format-out("\nTotal singletons: %d", size(data));
     for (item in alist)
       let name = first(item);
       let count = rest(item);
       format-out("\n%s\t%d", name, count)
     end;
    end
  end;
  values()
end;
*/

define method make (cl == <type-estimate-limited-instance>, 
                    #next real-maker, #rest keys, #key singleton, #all-keys)
 => (object :: <type-estimate-limited-instance>)
  local method just-cons-it ()  => (te :: <type-estimate-limited-instance>)
          // Resign ourselves to constructing a new one; supply class: key.
          // *** Performance analysis code, normally dead.
          // *limited-instances* := pair(singleton, *limited-instances*);
          apply(real-maker, cl, class: &object-class(singleton), keys)
        end;
  select (singleton by instance?)
    <boolean>, <number> // , <&callable-object>
      // Look up singletons of booleans in the cons cache: there can only be 
      //   2 of them, and we were consing 2328 of them!
      // Only 36 numbers in Dylan library, but some duplication.  A numerics library,
      //   of course, would have many more numeric constants in it.
      // *** Metering of Dylan library shows 3174 singleton functions; gotta be some
      //     duplication there?
      => let te-cons-cache = library-type-estimate-cons-cache(current-library-description());
         let cache-val     = element(te-cons-cache, singleton, default: #f);
         if (cache-val)
           // Found it in the cons cache, so go with that.
           cache-val
         else
           // Cons cache miss, so make one, cache it, and return it.
           let new-te = just-cons-it();
           te-cons-cache[singleton] := new-te;
           new-te
         end;
    otherwise
      // Everything else is just consed and not encached.
      // Warning: Note that if you attempt to use the cons cache for classes here,
      // you'll conflict with <type-estimate-class>, which keys on classes too.
      => just-cons-it();
  end
end;

define method print-type-estimate-internals
    (teli :: <type-estimate-limited-instance>, #key stream) => ()
  format(stream, "singleton(%s)", type-estimate-debug-name(type-estimate-singleton(teli)))
end;

///
/// Limited collection <type-estimate>s.
///

define type-class <type-estimate-limited-collection> (<type-estimate-limited>)
  keyword class: = dylan-value(#"<collection>");
  keyword root:  = dylan-value(#"<collection>");
  type-slot type-estimate-of-variable :: false-or(<type-variable>) = #f,
    init-keyword: of:;
  constant slot type-estimate-concrete-class /* :: <&class> */,
    required-init-keyword: concrete-class:;
  constant slot type-estimate-size :: false-or(limited(<integer>, min: 0)) = #f,
    init-keyword: size:;
  constant slot type-estimate-dimensions :: false-or(limited(<sequence>, 
                                                             of: <integer>
                                                               // limited(<integer>, min: 0)
                                                               )),
    init-value: #f, init-keyword: dimensions:;
end;

define method make (lc :: subclass(<type-estimate-limited-collection>), 
                    #rest the-keys, 
                    #key class, concrete-class, of, size, dimensions, #all-keys)
 => (te :: <type-estimate>)
  assert(of | size | dimensions, 
         "Must supply at least 1 of of:, size:, or dimensions:.");
  // *** Awaits <&stretchy-collection>.
  //assert(~size | ~^subtype?(class, dylan-value(#"<stretchy-collection>")), 
  //       "size: %= given, but class: %= is stretchy.", size, class)
  assert(~size | ~dimensions,
         "Cannot give both size: %= and dimensions: %=.", size, dimensions);
  assert(~dimensions | (class & ^subtype?(class, dylan-value(#"<array>"))),
         "dimensions: %= given, but class: %= is not a subclass of <array>.",
          dimensions, class);
  // Add root: and coerce of:.
  apply(next-method, lc, 
        root:           dylan-value(#"<collection>"),
        class:          class,
        concrete-class: concrete-class,
        of:             as-false-or-type-variable(of),
        size:           size,
        dimensions:     dimensions, 
        the-keys)
end;

define method print-type-estimate-internals
    (lc :: <type-estimate-limited-collection>, #key stream) => ()
  format(stream, "limited(%s", 
         type-estimate-debug-name(type-estimate-class(lc)));
  ~type-estimate-of(lc)         | format(stream, ", of: %s", 
                                         type-estimate-of(lc));
  ~type-estimate-size(lc)       | format(stream, ", size: %s", 
                                         type-estimate-size(lc));
  ~type-estimate-dimensions(lc) | format(stream, ", dimensions: %s", 
                                         type-estimate-dimensions(lc));
  write-element(stream, ')')
end;

///
/// Limited function <type-estimate>s.  This limits <function>, in that
/// it contains extra information about arguments & values.  But any instance
/// of this will be a subtype of the class estimate for <function>.  Actually,
/// nowadays it limits <callable-object>, which is a superclass of <function>.
/// That's so it can model primitives and odd things like that.
///

/// TODO: TURN THIS ON
define constant <type-variable-table> = <object-table>;
//   = limited(<table>, of: <type-variable>);

define type-class <type-estimate-limited-function> (<type-estimate-limited>)
  // TODO: callable-object is no longer valid superclass model
  keyword class: /* = dylan-value(#"<callable-object>") */;
  type-slot type-estimate-requireds-variable :: <type-variable-vector> = #[],
    init-keyword: requireds:;
  constant slot type-estimate-rest?     :: <boolean> = #t,
    // DRM, p. 83: "... types of rest parameters cannot be declared."
    init-keyword: rest?:;
  // *** Make this always a table, but sometimes empty?
  type-slot type-estimate-keys-variable :: false-or(<type-variable-table>) = #f,
    init-keyword: keys:;
  constant slot type-estimate-all-keys? :: <boolean> = #f,
    init-keyword: all-keys?:;
// #next can't have a type, either.  Can we infer one?  Special case?
  type-slot type-estimate-values-variable :: <type-variable>
    = make(<type-variable>,  // *** Need to be a variable?
           contents: make(<type-estimate-values>)),
    init-keyword: vals:;
end;

define method make
    (lf :: subclass(<type-estimate-limited-function>),
     #rest the-keys, 
     #key requireds :: false-or(<simple-object-vector>), 
          keys :: false-or(<table>), vals, 
     #all-keys)
 => (te :: <type-estimate>)
  // Coerce keywords to type variables and collections thereof.
  apply(next-method, lf, 
        // TODO: callable-object is no longer valid superclass model
        // root:      dylan-value(#"<callable-object>"),
        requireds: case
                     requireds => as-false-or-type-variable-coll(requireds);
                     otherwise => #[];
                   end,
        keys:      as-false-or-type-variable-coll(keys),
        vals:      select (vals by instance?)
                     <type-variable> => vals;
                     otherwise       => make(<type-variable>, contents: vals);
                   end,
        the-keys)
end;

define method print-type-estimate-internals
    (tef :: <type-estimate-limited-function>, #key stream) => ()
  // E.g., <lambda>(<integer>, <stream>) -> values(<integer>)
  // Well, this certainly looks complicated, doesn't it?
  let predecessors? = #f;
  local method maybe-comma()
          // The syntax is less than transparent about commas here!
          // Thus, print-separated-collection isn't quite up to it.
          when (predecessors?) write(stream, ", ") end;
          predecessors? := #t
        end;
  format(stream, "limited(%s, arguments: (", 
         type-estimate-debug-name(type-estimate-class(tef)));
  unless (empty?(type-estimate-requireds(tef)))
    maybe-comma();                                           // Required args
    print-separated-collection(type-estimate-requireds(tef), stream: stream)
  end;
  when (type-estimate-rest?(tef))
    maybe-comma();
    format(stream, "#rest %s", 
           type-estimate-debug-name(dylan-value(#"<object>"))) // #rest present
  end;
  when (type-estimate-keys(tef))
    maybe-comma();
    write(stream, "#key ");                                  // Keyword args
    let prev-key? = #f;                                      // For commas
    map-table(type-estimate-keys(tef),
              method (key, key-type)
                when (prev-key?)
                  write(stream, ", ")                        // Comma between
                end;
                format(stream, "%s :: %s", key, key-type);
                prev-key? := #t
              end)
  end;
  when (type-estimate-all-keys?(tef))
    maybe-comma();
    write(stream, "#all-keys")                               // #all-keys
  end;
  format(stream, "), values: %s)", type-estimate-values(tef))
end;

/// 
/// Union <type-estimate>s.
///

define type-class <type-estimate-union> (<type-estimate>)
  type-slot type-estimate-unionees-variable :: <type-variables>,
    required-init-keyword: unionees:;
end;

define method make
    (teu :: subclass(<type-estimate-union>), 
     #rest the-keys, #key unionees, #all-keys)
 => (te :: <type-estimate>)
  // Enwrap the unionees in type variables.
  let unionees :: <unionee-sequence> = as(<unionee-sequence>, unionees);
  apply(next-method, teu, unionees: as-false-or-type-variable-coll(unionees),
        the-keys)
end;

define method print-type-estimate-internals(un :: <type-estimate-union>, 
                                            #key stream) => ()
  write(stream, "type-union(");
  print-separated-collection(type-estimate-unionees(un), stream: stream);
  write-element(stream, ')')
end;

///
/// Bottom <type-estimate>s.
///

define type-class <type-estimate-bottom> (<type-estimate>)
  // No interesting structure, sort of by definition!
end;

define constant $bottom-cons-cache-key$ :: <pair> = list(#"bottom");

define method make (cl == <type-estimate-bottom>, #rest keys, #key, #all-keys)
 => (te :: <type-estimate-bottom>)
  // Use the canonical one in the cons cache, if it's there.
  // Note that since there's no model-object to take model-library of, 
  // we pretty much have to do current-library-description().
  let te-cons-cache = library-type-estimate-cons-cache(current-library-description());
  let cache-val     = element(te-cons-cache, $bottom-cons-cache-key$, 
                              default: #f);
  if (cache-val)
    cache-val
  else
    let the-bottom = next-method();
    te-cons-cache[$bottom-cons-cache-key$] := the-bottom;
    the-bottom
  end
end;

define method print-type-estimate-internals(b :: <type-estimate-bottom>, 
                                            #key stream) => ()
  format(stream, "<bottom>")
end;

///
/// How to convert from a model type to a <type-estimate>.
///

define macro as-type-estimate-rules-definer
  // Expand a bunch of rules into methods for as(<type-estimate>, ...).
  { define as-type-estimate-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates an as(<type-estimate>, ...) method.
  { ?t:name :: ?t-type:* -> ?expr:expression }
    => { define method as (te == <type-estimate>, ?t :: ?t-type) 
           => (result :: <type-estimate>)
           ?expr
         end }
end;

define as-type-estimate-rules
  t :: <&top-type>                -> make(<type-estimate-top>);
  t :: <&bottom-type>             -> make(<type-estimate-bottom>);
  t :: <&class>                   -> make(<type-estimate-class>, class: t);
  t :: <&raw-type>                -> make(<type-estimate-raw>, raw: t);
  t :: <&raw-aggregate-type>      -> make(<type-estimate-raw>, raw: t);
  // *** Is <&values> a rep'n of multiple values?
  t :: <&limited-integer>         -> make(<type-estimate-limited-integer>, 
                                          min: ^limited-integer-min(t),
                                          max: ^limited-integer-max(t));
  // *** Limited collections, ranges
  t :: <&subclass>                -> make(<type-estimate-limited-class>, // *** Use cons cache?
                                          subclass: ^subclass-class(t));
  t :: <&limited-collection-type> -> make(<type-estimate-limited-collection>, // *** Use cons cache?
                                          class:          ^limited-collection-class(t),
                                          concrete-class: ^limited-collection-concrete-class(t),
                                          of:             as(<type-estimate>, ^limited-collection-element-type(t)),
                                          // *** Should add element-type-fill?
                                          size:           ^limited-collection-size(t),
                                          dimensions:     ^limited-collection-dimensions(t) &
                                                            as(limited(<vector>, of: <integer>),
                                                               ^limited-collection-dimensions(t)));
  t :: <&singleton>               -> make(<type-estimate-limited-instance>, // *** Use cons cache?
                                          singleton: ^singleton-object(t));
  t :: <&union>                   -> make(<type-estimate-union>,
                                          unionees: list(as(<type-estimate>,
                                                            ^union-type1(t)),
                                                         as(<type-estimate>,
                                                            ^union-type2(t))));
end;

///
/// How to convert from a <type-estimate> to a model type.
///

define macro as-model-type-rules-definer
  // Expand a bunch of rules into methods for as(<&type>, ...).
  { define as-model-type-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates an as(<&type>, ...) method.
  { ?t:name :: ?t-type:* -> ?expr:expression }
    => { define method as (te == <&type>, ?t :: ?t-type) => (result :: <&type>)
           ?expr
         end }
end;

define function as-model-type-error(te :: <type-estimate>, reason :: <string>)
  error("Type estimate %= cannot be coerced to a model type: %s", te, reason)
end;

define as-model-type-rules
  t :: <type-estimate-top>              -> dylan-value(#"<top>");
  t :: <type-estimate-class>            -> type-estimate-class(t);
  t :: <type-estimate-raw>              -> type-estimate-raw(t);
  t :: <type-estimate-values>           -> as-model-type-error( 
                                              t, "not a Dylan type");
  t :: <type-estimate-limited-integer>  -> ^limited-integer(
                                              min: type-estimate-min(t),
                                              max: type-estimate-max(t));
  // *** Limited collections, ranges
  t :: <type-estimate-limited-class>    -> ^subclass(type-estimate-subclass(t));
  t :: <type-estimate-limited-instance> -> ^singleton(
                                              type-estimate-singleton(t));
  t :: <type-estimate-limited-function> -> type-estimate-class(t); // <= <callable-object>
  t :: <type-estimate-union>            -> apply(^type-union,
                                                 // Use reduce & compose?
                                                 map(curry(as, <&type>), 
                                                    type-estimate-unionees(t)));
  t :: <type-estimate-bottom>           -> dylan-value(#"<bottom>");
end;
