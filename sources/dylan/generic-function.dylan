Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// <GENERIC-FUNCTION>

// BOOTED: define ... class <generic-function> ... end;


define function generic-function-sealed? (g :: <generic-function>) => (well?)
  if (instance?(g, <incremental-generic-function>))
    let g :: <incremental-generic-function> = g;
    incremental-gf-sealed?(g)
  else
    #t
  end if
end function;


define inline method type-complete? (g :: <generic-function>) => (well? :: <boolean>)
  (~instance?(g, <incremental-generic-function>) | incremental-gf-signatured?(g))
  & 
  type-complete?(function-signature(g))
end method;


define method recompute-type-complete! (g :: <generic-function>) => (well? :: <boolean>)
  (~instance?(g, <incremental-generic-function>) | incremental-gf-signatured?(g))
  & 
  recompute-type-complete!(function-signature(g))
end method;


define method reduce-incomplete-classes (f :: <function>, g :: <generic-function>, ans)
 => (ans)
  reduce-incomplete-classes(f, function-signature(g), ans)
end method;


define method map-congruency-classes (f :: <function>, g :: <incremental-generic-function>)
 => ()
  map-congruency-classes(f, function-signature(g));
  map(curry(map-congruency-classes, f), generic-function-methods(g));
  do(method(x) map-congruency-classes(f, first(x)) end, generic-function-incomplete-methods(g));
  map(curry(map-congruency-classes, f), generic-function-incomplete-domains(g));
  (iterate loop (d :: false-or(<domain>) = incremental-gf-domain-info(g))
     if (d)
       let d :: <domain> = d;
       map-congruency-classes(f, d);
       loop(domain-next(d))
     end if
  end iterate);
end method;


define inline function incremental-gf-library (g :: <incremental-generic-function>)
 => (l :: <library>)
  home-library(incremental-gf-module(g))
end function;


define leaf packed-slots incremental-gf-properties (<incremental-generic-function>, <object>)
  boolean slot incremental-gf-sealed? = #f,
    init-keyword: sealed?:;
  boolean slot incremental-gf-method-complete? = #t;
  boolean slot incremental-gf-signatured? = #t;
end packed-slots;


define method generic-function-mandatory-keywords
    (gf :: <generic-function>) => (keywords :: false-or(<collection>))
  let signature = gf.function-signature;
  signature.signature-key? & signature.signature-keys
end method generic-function-mandatory-keywords;


define method make (c == <generic-function>, #rest args,
		    #key required, key, key-types, rest?, values, rest-value)
 => (i :: <incremental-generic-function>)
  apply(make, <incremental-generic-function>, module: $runtime-module, args)
end method;



define method initialize
    (generic-function :: <generic-function>, 
     #rest all-keys, 
     #key signature, required, key, key-types, rest?, values: vals = #[], rest-value = <object>)
 => ()
  let e = if (key)
	    if (rest?)
	      "attempt to create a generic function with both optionals and keyword parameters: %=."
	    elseif (key-types)
	      if (~instance?(key-types, <sequence>) | any?(method(x) ~instance?(x, <type>) end, key-types))
		"generic function key-types must be a sequence of types: %=."
	      elseif (~instance?(key, <sequence>) | any?(method (x) ~instance?(x, <symbol>) end, key))
		"generic function key: must be a sequence of symbols: %=."
	      elseif (size(key) ~== size(key-types))
		"generic function key-types: and key: must be the same size: %=."
	      else #f
	      end if
	    else #f
	    end if
	  elseif (key-types)
	    "attempt to create an unkeyed generic function with key-types: %=."
	  else #f
	  end if;
  if (e)
    error(make(<argument-error>, format-string: e, format-arguments: vector(all-keys)))
  end if;

  next-method();

  if (~signature)
    function-signature(generic-function) 
      := if (key)
	   apply(make, <signature>, 
		 required: required,
		 values: vals,
		 rest-value: rest-value, rest?: rest?, 
		 keys: as(<simple-object-vector>, key),
		 key?: #t,
		 key-types: if (key-types) as(<simple-object-vector>, key-types) else #[] end,
		 all-keys)
	 else
	   apply(make, <signature>, 
		 required: required,
		 values: vals, 
		 rest?: rest?, rest-value: rest-value,
		 all-keys)
	 end if
  end if;
  finalize-generic-function(generic-function);
end method initialize;


define method initialize (gf :: <incremental-generic-function>, #rest all-keys, #key)
 => ()
  next-method();
  apply(initialize-packed-slots, gf, all-keys);
  map-congruency-classes(method (c)
			   let ic :: <implementation-class> = class-implementation-class(c);
			   if (class-incremental?(ic) | ~class-complete?(ic))
			     iclass-dependent-generics(ic) := add-new!(iclass-dependent-generics(ic), gf)
			   end if
			 end method,
			 gf);
  // gf.incremental-gf-type-complete? := type-complete?(function-signature(gf));
end method;


define method finalize-generic-function (generic-function :: <generic-function>) => ()
  decache-gf(generic-function);
  // generic-function.discriminator := $absent-engine-node;
  primitive-set-generic-function-entrypoints(generic-function);
end method finalize-generic-function;


define method decache-gf (g :: <generic-function>) => ()
  g.discriminator := $absent-engine-node;
  let cache = %gf-cache(g);
  if (instance?(cache, <gf-cache-info>))
    let cache :: <gf-cache-info> = cache;
    for (x in gf-cache-info-users(cache))
      if (x)
	let x :: <cache-header-engine-node> = x;
	cache-header-engine-node-next(x) := $absent-engine-node;
      end 
    end for
  end if
end method;



define abstract class <object-incomplete> (<condition>)
  constant slot incomplete-object, required-init-keyword: object:;
end class;


define abstract class <incomplete-error> (<object-incomplete>, <error>)
end class;


define class <function-type-incomplete-error> (<simple-condition>, <incomplete-error>)
  // A function is type-incomplete because one or more additional types are incomplete.
  constant slot incomplete-types :: <collection>, required-init-keyword: types:;
end class;


define function call-to-type-incomplete-generic (g :: <generic-function>, mepargs :: <simple-object-vector>)
  let args = reconstruct-args-from-mepargs(g, mepargs);
  let incomplete = incomplete-classes(function-signature(g));
  make(<function-type-incomplete-error>,
       object: g,
       types: incomplete-classes(function-signature(g)),
       format-string: "Call to type-incomplete generic %= on args %=.\n"
	 "The generic cannot be used yet because the types %= have not been fully computed.",
       format-arguments: vector(g, args, incomplete))
end function;


define function report-generic-incomplete (g :: <generic-function>, name, meth)
  bletch(make(<function-type-incomplete-error>,
	      object: g,
	      types: incomplete-classes(function-signature(g)),
	      format-string: "attempt to %s on type-incomplete generic: %=\n  (method: %=)",
	      format-arguments: vector(name, g, meth)))
end function;


define inline function check-generic-incomplete 
    (gf :: <generic-function>, name :: <string>, meth :: false-or (<method>)) => ()
  unless (gf.type-complete?)
    report-generic-incomplete(gf, name, meth)
  end;
end;


define function locate-method 
    (generic-function :: <generic-function>, the-specializers :: <simple-object-vector>)
 => (m :: false-or(<method>), idx :: <integer>)
  block (return)
    let nspecs :: <integer> = size(the-specializers);
    for (a-method :: <method> in generic-function.generic-function-methods, 
	 i :: <integer> from 0)
      local method loop (specnum :: <integer>)
	      if (specnum == nspecs) 
		return(a-method, i)
	      elseif (same-specializer?(%method-specializer(a-method, specnum),
					  vector-element(the-specializers, specnum)))
		loop(specnum + 1)
	      end if
	    end method;
      loop(0);
    end for;
    values(#f, -1)
  end block
end function;


define method find-method
    (generic-function :: <generic-function>, the-specializers :: <sequence>)
 => (m :: false-or(<method>))
  check-generic-incomplete (generic-function, "find-method", #f) ;
  values(if (instance?(the-specializers, <simple-object-vector>))
	   locate-method(generic-function, the-specializers)
	 else
	   apply(method(#rest v) locate-method(generic-function, v) end, the-specializers)
	 end if)
end method find-method;


define method congruent? (f1 :: <generic-function>, f2 :: <method>)
 => (b :: <boolean>, reason)
  congruent?(function-signature(f1), function-signature(f2))
end method;



define variable note-generic-function-incomplete-method :: <function>
  = ignore;

define variable note-generic-function-incomplete-domain :: <function>
  = ignore;

define variable remove-generic-function-incomplete-method :: <function>
  = ignore;

define variable remove-generic-function-incomplete-domain :: <function>
  = ignore;


define method copy-over-without (seq :: <simple-object-vector>, idx :: <integer>)
 => (new :: <simple-object-vector>)
  let n :: <integer> = size(seq) - 1;
  let new :: <simple-object-vector> = make(<simple-object-vector>, size: n);
  for (i :: <integer> from 0 below idx) vector-element(new, i) := vector-element(seq, i) end;
  for (i :: <integer> from idx below n)
    vector-element(new, i) := vector-element(seq, i + 1)
  end for;
  new
end method;


define method copy-over-without (seq :: <list>, idx :: <integer>)
  if (idx == 0)
    tail(seq)
  else
    local method loop (prev :: <pair>, l :: <list>, i :: <integer>)
	    if (i == idx) 
	      tail(prev) := tail(l)
	    else
	      loop(tail(prev) := pair(head(l), #()), tail(l), i + 1)
	    end if
	  end method;
    let top = pair(head(seq), #());
    loop (top, tail(seq), 1);
    top
  end if
end method;


define class <sealed-generic-function-error> (<sealed-object-error>, <simple-condition>)
  constant slot sealed-generic-function-error-generic :: <generic-function>,
    required-init-keyword: generic-function:;
  slot sealed-generic-function-error-operation, init-keyword: operation:;
  slot sealed-generic-function-error-arguments :: <sequence>, init-keyword: arguments:;
end class;

define function add-method-internal (g :: <generic-function>, m :: <method>, lib :: <library>,
				     check-congruency? :: <boolean>, check-sealing? :: <boolean>)
  => (new-value, condition);
  let reason1 = check-sealing? & method-not-frobbable?(g, m, lib, "add-method");
  if (instance?(reason1, <sealed-generic-function-error>))
    sealed-generic-function-error-operation(reason1) := add-method;
    sealed-generic-function-error-arguments(reason1) := vector(g, m);
  end;
  if (instance?(reason1, <error>))
    // If the reason we can't add is an error, stop now.  Otherwise we keep going
    // (hopefully adding the method) and just signal the condition.
    values(#f, reason1)
  elseif (~type-complete?(g) | ~type-complete?(m))
    note-generic-function-incomplete-method(g, m, lib);
    values(#f, #f)
  else
    let (well?, reason2) = if (check-congruency?) congruent?(g, m) else values(#t, #f) end;
    if (~well?)
      values(#f, make(<argument-error>,
		      format-string: "the method %= is not congruent with generic function %= - %s.",
		      format-arguments: list(m, g, reason2)))
    else 
      let old-meth = add-method-internal-internal(g, m, lib);
      values(old-meth, reason1)
    end if
  end if
end function;


define function method-not-frobbable? (g :: <generic-function>, frob, lib :: <library>, opstring)
 => (reason :: false-or(<condition>))
  if (generic-function-sealed?(g))
    // If sealed, we can add the method only if it's an incremental generic and
    // it's being added by the same library.
    if (instance?(g, <incremental-generic-function>) & incremental-gf-library(g) == lib)
      #f
    else
      make(<sealed-generic-function-error>,
	   generic-function: g,
	   format-string: "Cannot %s %= in %= to sealed generic %=.",
	   format-arguments: vector(opstring, frob, lib, g))
    end if
  else
    domain-conflict?(g, frob, lib, #t, opstring)
  end;
end function;

define constant *register-subclass-dependent-generics-during-add-method?* = #f;

define function add-method-internal-internal  (g :: <generic-function>, 
					       m :: <method>, lib :: <library>)
  let specs :: <simple-object-vector> = function-specializers(m);
  let (old-meth, idx :: <integer>) = locate-method(g, specs);
  when (*register-subclass-dependent-generics-during-add-method?*)
    map-congruency-classes-sov
      (method (class :: <class>)
	 unless (iclass-subclasses-fixed?(class-implementation-class(class)))
	   %register-subclass-dependent-generic(g, class);
	 end unless;
       end method,
       specs,
       size(specs));
  end when;
  let meths = generic-function-methods(g);
  if (old-meth)
    if (instance?(g, <incremental-generic-function>))
      let g :: <incremental-generic-function> = g;
      let glib :: <library> = incremental-gf-library(g);
      let libs :: <simple-object-vector> = incremental-gf-method-libraries(g);
      let nlibs :: <integer> = size(libs);
      if (idx < nlibs)
	if (lib ~== glib)
	  // prev and new methods have library explicitly stored.
	  libs[idx] := lib;
	  meths[idx] := m;
	else
	  // prev meth had library explicitly stored, new doesn't.
	  for (i :: <integer> from idx below nlibs - 1) meths[i] := meths[i + 1] end;
	  meths[nlibs - 1] := m;
	  incremental-gf-method-libraries(g) := copy-over-without(libs, idx);
	end if
      elseif (lib ~== glib)
	// new meth has library explicit, prev did not.
	incremental-gf-method-libraries(g) := concatenate(libs, vector(lib));
	if (idx ~== nlibs)
	  meths[idx] := meths[nlibs];
	  meths[nlibs] := m;
	end if;
      else
	// both prev and new methods have same library as gf.
	meths[idx] := m;
      end if;
    else
      meths[idx] := m;
    end if
  else
    // adding a new method.
    if (lib & instance?(g, <incremental-generic-function>))
      %add-method-and-library(g, m, lib)
    else
      generic-function-methods(g)
	:= concatenate(generic-function-methods(g), vector(m));
    end if;
  end if;
  g.finalize-generic-function;
  old-meth
end function;



define method add-method
    (a-generic-function :: <generic-function>, a-method :: <method>)
  // TODO:  work completeness check into the locked part.
  check-generic-incomplete (a-generic-function, "add-method", a-method) ;
  let (ans, condition)
    = (with-object-lock (a-generic-function)
        add-method-internal(a-generic-function, a-method, $runtime-library, #t, #t)
      end with-object-lock);
  if (condition) bletch(condition) end;
  values(a-method, ans)
end method add-method;



define function %add-method-and-library (g :: <incremental-generic-function>,
					 m :: <method>, lib :: <library>)
 => ()
  incremental-gf-method-libraries(g) 
    := concatenate(vector(lib), incremental-gf-method-libraries(g));
  let meths = generic-function-methods(g);
  if (instance?(meths, <list>))
    let meths :: <list> = meths;
    generic-function-methods(g) := pair(m, meths);
  else
    generic-function-methods(g)
      := replace-subsequence!(generic-function-methods(g), vector(m), start: 0, end: 0);
  end if;
end function;


define function %add-a-method (g :: <generic-function>, m :: <method>, lib :: <library>, 
			       check-congruency? :: <boolean>, check-sealing? :: <boolean>,
			       add-method-domain? :: <boolean>)
 => ()
  let lossage = (with-object-lock (g)
		   let (ans, condition) 
		   = add-method-internal(g, m, lib, check-congruency?, check-sealing?);
		 let more-lossage =
		   if (add-method-domain?
			 & ~instance?(condition, <error>)
			 & instance?(g, <incremental-generic-function>))
		     let g :: <incremental-generic-function> = g;
		     %add-method-domain(g, m, lib, check-sealing?)
		   else
		     #()
		   end if;
		 if (condition) pair(condition, more-lossage) else more-lossage end;
		end with-object-lock);
  if (lossage ~== #())
    if (instance?(g, <incremental-generic-function>)
          & (incremental-gf-library(g) == lib 
               | incremental-gf-module(g) == $runtime-module))
      block ()
        bletch-stack(lossage);
      exception (<simple-restart>,
                 init-arguments: 
                   vector(format-string: 
                            "Redefine %=, discarding non-congruent methods.",
                          format-arguments: 
                            vector(g)))
        %redefine-generic-using-method(g, m, lib, add-method-domain?);
      end;
    else
      bletch-stack(lossage);
    end;
  end;
end function;

define function %redefine-generic-using-method
    (g :: <generic-function>, m :: <method>, lib :: <library>, 
       add-method-domain? :: <boolean>)
 => ()
  let implicit-sig = implicit-generic-signature(function-signature(m));
  %redefine-generic
    (g, debug-name(g), $runtime-module, implicit-sig, #f);
  %add-a-method
    (g, m, lib, #f, #f, add-method-domain?);
end function;

define function implicit-generic-signature 
    (sig :: <signature>) => (implict-sig :: <signature>)
  let nrequired = signature-number-required(sig);
  case
    signature-key?(sig)
      => make(<signature>,
              required:  nrequired,
              key?:      #t,
              keys:      #[],
              key-types: #[]);
    signature-rest?(sig)
      => make(<signature>,
              required: nrequired,
              rest?:    #t);
    otherwise
      => make(<signature>,
              required: nrequired);
  end;
end function;

// Add a dynamically computed method to a sealed generic function.
define function %add-dynamic-method (g :: <sealed-generic-function>, m :: <method>) 
 => ()
  %add-a-method(g, m, $runtime-library, #t, #f, #f)
end function;


define function %add-method (g :: <generic-function>, m :: <method>, lib :: <library>)
 => ()
  %add-a-method(g, m, lib, #f, #t, #f)
end function;


// This is %add-method when we know there can't be any preexisting method on the
// same specializers.  This could be used when some specializer of the method
// is defined in the library, guaranteeing this situation.
define function %add-nonsiblinged-method (g :: <generic-function>, 
					  m :: <method>, lib :: <library>)
 => ()
  let lossage :: <list>
    = with-object-lock (g)
        if (instance?(g, <incremental-generic-function>))
	  let g :: <incremental-generic-function> = g;
	  %add-method-and-library(g, m, lib);
	  g.finalize-generic-function;
	  #()
	else
	  // A <sealed-generic-function> here means that this is a sideways
	  // definition on a compiler-open generic.  So we just add it and
	  // look the other way.
	  let g :: <sealed-generic-function> = g;
	  let meths = generic-function-methods(g);
	  if (instance?(meths, <list>))
	    generic-function-methods(g) := pair(m, meths)
	  else
	    let methds :: <simple-object-vector> = meths;
	    generic-function-methods(g) := concatenate(vector(m), meths)
	  end if;
	  finalize-generic-function(g);
	  #()
	end if
      end with-object-lock;
  bletch-stack(lossage);
end function %add-nonsiblinged-method;



// For users.
define function remove-method (g :: <generic-function>, m :: <method>)
 => (m :: <method>)
  %remove-method-from-library(g, m, $runtime-library, #t, \==);
  m
end function;


// For the environbment?
define function remove-method-via-specializers (g :: <generic-function>, specs, lib :: <library>)
 => (removed? :: false-or(<method>));
  %remove-method-from-library(g, specs, lib, #t, domain-match?)
end function;


// For dirty work.
define function %remove-method (g :: <generic-function>, m :: <method>)
 => (removed? :: false-or(<method>));
  %remove-method-from-library(g, m, $runtime-library, #f, \==)
end function;


// this is the function definition that causes the "live on entry to lambda"
// serious warning.
define function %remove-method-from-library (g :: <generic-function>, frob, lib, 
					     check-sealing?, test :: <function>)
 => (removed? :: false-or(<method>));
  let (removed?, lossage)
    = (with-object-lock (g)
	 let (ans :: false-or(<method>), condition)
	   = remove-method-internal(g, frob, lib, check-sealing?, test);
	 unless (~ans | instance?(condition, <error>))
	   let g :: <incremental-generic-function> = g;
	   let ans :: <method> = ans;
	   %remove-method-domain(g, ans, lib)
	 end unless;
	 values(ans, condition)
       end with-object-lock);
  if (lossage) bletch(lossage) end;
  removed?
end function;


define function remove-method-internal (g :: <generic-function>, frob, lib, check-sealing?, test :: <function>)
  => (removed? :: false-or(<method>), condition :: false-or(<condition>));
  let reason1 = check-sealing? & method-not-frobbable?(g, frob, lib, "remove-method");
  if (instance?(reason1, <sealed-generic-function-error>))
    sealed-generic-function-error-operation(reason1) := remove-method;
    sealed-generic-function-error-arguments(reason1) := vector(g, frob);
  end;
  if (instance?(reason1, <error>))
    values(#f, reason1)
  else
    block (return)
      for (m :: <method> in generic-function-methods(g), i :: <integer> from 0)
	if (test(m, frob))
	  generic-function-methods(g) := copy-over-without(generic-function-methods(g), i);
	  if (instance?(g, <incremental-generic-function>))
	    let g :: <incremental-generic-function> = g;
	    let libs :: <simple-object-vector> = incremental-gf-method-libraries(g);
	    if (i < size(libs))
	      incremental-gf-method-libraries(g) 
		:= copy-over-without(incremental-gf-method-libraries(g), i);
	    end if;
	    return(m, reason1)
	  end if
	end if
      end for;
      values(remove-generic-function-incomplete-method(g, frob, lib, test), reason1);
    end
  end if
end function;



define generic applicable-method? (function :: <function>, #rest sample-arguments);


define method applicable-method? 
    (function :: <method>, #rest sample-arguments) => (result :: <boolean>)
  let sig :: <signature> = function-signature(function);
  let size-given :: <integer> = sample-arguments.size;
  let num-req :: <integer> = signature-number-required(sig);
  if (signature-optionals?(sig))
    size-given >= num-req
  else
    size-given == num-req
  end if
    & applicable-method-assuming-number-required?(function, sample-arguments)
end method applicable-method?;


define method applicable-method?
    (gf :: <generic-function>, #rest sample-arguments) => (result :: <boolean>)
  check-generic-incomplete (gf, "applicable-method?", #f) ;
  let sig :: <signature> = function-signature(gf);
  let size-given :: <integer> = sample-arguments.size;
  let num-req :: <integer> = signature-number-required(sig);
  if (signature-optionals?(sig))
    size-given >= num-req
  else
    size-given == num-req
  end if
    & block (return)
	for (m :: <method> in generic-function-methods(gf))
	  if (applicable-method-assuming-number-required?(m, sample-arguments))
	    return(#t)
	  end if
	end for;
	#f
      end
end method;


////
//// RETURNS APPLICABLE METHODS SORTED IN ORDER OF SPECIFICITY
////

define generic sorted-applicable-methods (generic-function, #rest arguments)
 => (ordered-unambiguous :: <sequence>, unordered-ambiguous :: <collection>);

define method sorted-applicable-methods
    (generic-function :: <generic-function>, #rest arguments) 
 => (ordered-unambiguous :: <list>, unordered-ambiguous :: <list>);
  check-generic-incomplete (generic-function, "sorted-applicable-methods", #f) ;
  compute-sorted-applicable-methods(generic-function, arguments)
end method sorted-applicable-methods;

define method function-specializers (gf :: <generic-function>, #next next-method)
  check-generic-incomplete (gf, "function-specializers", #f) ;
  next-method ()
end;

define method function-return-values (gf :: <generic-function>, #next next-method) =>
    (return-value-types :: <sequence>, rest-return-value :: false-or(<type>))
  check-generic-incomplete (gf, "function-return-values", #f) ;
  next-method ()
end;

// eof
 
