module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// CLASS SYMBOL TABLE

// TODO: THREAD SAFE
define /* thread */ variable *class-symbol-table* :: false-or(<string-table>) = #f;

define method class->variable 
    (class :: <class>) 
 => (binding :: <byte-string>, module :: <byte-string>, library :: <byte-string>)
  let class-binding-name = debug-name(class);
  let class-module       = class-module(class);
  let class-module-name  = namespace-name(class-module);
  let class-library      = home-library(class-module);
  let class-library-name = namespace-name(class-library);
  values(class-binding-name, class-module-name, class-library-name)
end method;

define method insert-class-binding (class :: <class>)
  let (binding, module, library)
    = class->variable(class);
  let bindings
    = lookup-class-bindings(module, library);
  bindings[binding] := class;
end method;

define method build-class-symbol-table ()
  let visited? = make(<object-set>);
  local method add-class (class)
	  add!(visited?, class);
	  insert-class-binding(class);
	  for (subclass in direct-subclasses(class))
	    maybe-add-class(subclass)
	  end for;
	end method,
        method maybe-add-class (class)
	  unless (member?(class, visited?))
	    add-class(class)
	  end unless;
	end method;
  add-class(<object>);
end method;

define method lookup-class-bindings 
    (module :: <byte-string>, library :: <byte-string>) => (bindings :: <string-table>)
  unless (*class-symbol-table*)
    *class-symbol-table* := make(<string-table>);
    build-class-symbol-table();
  end unless;
  let libraries
    = *class-symbol-table*;
  let modules            
    = element(libraries, library, default: #f)
	| (element(libraries, library) := make(<string-table>));
  let bindings
    = element(modules, module, default: #f)
	| (element(modules, module) := make(<string-table>));
  bindings
end method;

define method variable->class
    (binding :: <byte-string>, module :: <byte-string>, library :: <byte-string>)
 => (class :: <class>)
  let bindings = lookup-class-bindings(module, library);
  // TODO: should signal sensible error if not found
  bindings[binding]
end method;


define sealed generic class-library (x) => (l :: <library>);

define inline method class-library (x :: <class>) => (l :: <library>)
  home-library(class-module(x))
end method;

define inline method class-library (x :: <implementation-class>) => (l :: <library>)
  class-library(iclass-class(x))
end method;

define method initialize 
    (class :: <class>, #rest initargs,
     #key superclasses, slots :: <sequence> = #[], 
          inherited-slots :: <sequence> = #[], keywords :: <sequence> = #[],
          module :: <module> = $runtime-module)
 => ()
  invalidate-class-instance?-iep(class);
  next-method();
  *class-symbol-table* := #f;
  let iclass :: <implementation-class>
    = apply(make, <implementation-class>, 
	    class: class,
	    superclasses: checked-superclasses(class, superclasses, #t),
	    subjunctive-class-universe: $empty-subjunctive-class-universe,
	    slots: slots,
	    inherited-slots: inherited-slots,
	    keywords: keywords,
	    library: home-library(module),
	    initargs);
  install-implementation-class(iclass, $empty-subjunctive-class-universe);
  add-slot-methods(iclass, $empty-subjunctive-class-universe,
                   override-sealing?: module ~= $runtime-module);
  complete-dependent-generic-functions(iclass, $empty-subjunctive-class-universe);
end method initialize;


define class <sealed-class-error> (<sealed-object-error>, <simple-condition>)
end class;


define variable $sealed-class-checking-enabled? = #t;


// @@@@ Deal with sealing...
define function checked-superclasses (c :: <class>, superclasses, require-type-completeness?)
 => (v :: <simple-object-vector>)
  let v :: <simple-object-vector> = if (instance?(superclasses, <sequence>))
				      if (instance?(superclasses, <simple-object-vector>))
					superclasses
				      else
					as(<simple-object-vector>, superclasses)
				      end if
				    elseif (~superclasses)
				      vector(<object>)
				    else
				      vector(superclasses)
				    end if;
  let lib :: <library> = home-library(class-module(c));
  let losers :: <list> = #();
  do(method(x)
	 let super :: <implementation-class> 
	   = if (instance?(x, <class>)) class-implementation-class(x)
	     elseif (instance?(x, <implementation-class>)) x
	     else
	       error(make(<type-error>,
			  value: x, type: type-union(<class>, <implementation-class>),
			  format-string: "Some superclass specified for %=, %=, is not a class.",
			  format-arguments: vector(c, x)))
	     end if;
	 if (require-type-completeness?)
	   unless (super.iclass-type-complete?)
	     report-class-incomplete(c, "attempt to make a class of incompletely initialized superclasses: %=")
	   end;
	 end if;
	 if ($sealed-class-checking-enabled? & class-sealed?(super) & class-library(super) ~== lib)
	   losers := pair(x, losers) end;
     end,
     v);
  if (losers ~== #())
    error(make(<sealed-class-error>,
	       format-string: "Cannot create the class %= in %= because the superclasses %= "
		 "are sealed and not accessible to it.",
	       format-arguments: vector(debug-name(c), class-module(c), losers)))
  end if;
  v
end function;


define method initialize 
    (iclass :: <implementation-class>, 
     #rest initargs,
     #key subjunctive-class-universe = #f, 
          defer-cross-class-computations? = #f,
	  superclasses = #f,
          slots :: <sequence> = #[],
          inherited-slots :: <sequence> = #[],
          keywords :: <sequence> = #[])
 => ()
  next-method();
  apply(initialize-packed-slots, iclass, initargs);
  let class :: <class> = iclass-class(iclass);
  initialize-class-dispatch-keys(iclass);
  iclass.direct-superclasses := checked-superclasses(class, superclasses, #f);
  let slotvec :: <simple-object-vector>
    = map-as(<simple-object-vector>,
	     method (stuff)
	       if (instance?(stuff, <slot-descriptor>))
		 stuff
	       else
		 apply(create-slot-descriptor, class, stuff) 
	       end if
	     end method,
	     slots);
  iclass.direct-slot-descriptors   := slotvec;
  let inherited-slots-vector :: <simple-object-vector>
    = map-as(<simple-object-vector>,
	     method (inherited-slot)
	       if (instance?(inherited-slot, <inherited-slot-descriptor>))
		 inherited-slot
	       else
		 apply(make, <inherited-slot-descriptor>, owner: class, inherited-slot)
	       end if
	     end method,
	     inherited-slots);
  direct-inherited-slot-descriptors(iclass) := inherited-slots-vector;
  let init-arg-slots-vector :: <simple-object-vector>
    = map-as(<simple-object-vector>,
	     method (init-arg-slot)
	       if (instance?(init-arg-slot, <init-arg-descriptor>))
		 init-arg-slot
	       else
		 apply(make, <init-arg-descriptor>, owner: class, init-arg-slot)
	       end if
	     end method,
	     keywords);
  direct-initialization-argument-descriptors(iclass) := init-arg-slots-vector;

  if (~defer-cross-class-computations? | subjunctive-class-universe)
    let subjunctive-class-universe :: <subjunctive-class-universe>
      = subjunctive-class-universe 
          | if (class-implementation-class(class) == iclass)
	      $empty-subjunctive-class-universe
	    else
	      make-empty-subjunctive-class-universe()
	    end if;
    if (subjunctive-class-universe ~== $empty-subjunctive-class-universe)
      scu-entry(class, subjunctive-class-universe) := iclass
    end if;
    if (~defer-cross-class-computations?)
      do-implementation-class-cross-class-initializations
	(iclass, subjunctive-class-universe, recurse?: #t);
    end if;
  end if;
end method initialize;



define method do-implementation-class-cross-class-initializations (iclass :: <implementation-class>,
								   scu :: <subjunctive-class-universe>,
								   #key recurse? = #f)
 => ()
  if (every?(method (c :: <class>) iclass-type-complete?(class-implementation-class(c)) end,
	     direct-superclasses(iclass)))
    let (all-super-iclasses, mask) = compute-cpl-rcpl-stuff(iclass, scu);
    check-for-duplicated-slots(iclass.direct-slot-descriptors);
    check-for-illegally-inherited-slots(iclass.direct-slot-descriptors, all-super-iclasses);
    finalize-inheritance(iclass, all-super-iclasses, scu, mask);
    if (recurse?)
      // If we're now type complete, and have direect subclasses, some of them may
      // now be type-complete.
      for (subc :: <class> in direct-subclasses(iclass))
	do-implementation-class-cross-class-initializations(scu-entry(subc, scu), scu, recurse?: #t);
      end for;
    end if;
  else
    iclass-type-complete?(iclass) := #f;
  end if;
end method;


define method install-implementation-class (iclass :: <implementation-class>, 
					    u :: <subjunctive-class-universe>)
 => ()
  // Start with the backpointer sorts of things:
  // Install the indices which might need to be checked doing instance? of superclasses.
  // Adding these superfluously is harmless, it just might slow things down epsilonically.
  augment-superior-rcpls(iclass, u);
  let class :: <class> = iclass-class(iclass);
  class-implementation-class(class) := iclass;
  initialize-class-instance?-iep(class);
  // CALL MOVED FROM BEFORE ICLASS INSTALLATION
  // Now do subclasses.
  record-new-class-subclass-usage(iclass);
end method;



// define method all-subclasses (class :: <class>) => (classes :: <list>)
//   local method all-subclasses* (class :: <class>, ans :: <list>)
// 	  for (subc :: <class> in direct-subclasses(class),
// 	       ans = ans then all-subclasses*(subc, add-new(ans, subc)))
// 	  finally ans
// 	  end for
// 	end method;
//   all-subclasses*(class, #())
// //  local method all-subclasses* (class :: <class>)
// //	  remove-duplicates
// //	    (pair(class, 
// //		  concatenate
// //		    (map(all-subclasses, direct-subclasses(class)))));
// //	end method;
// //  remove(all-subclasses*(class), class)
// end method;

define function record-new-class-subclass-usage (iclass :: <implementation-class>) => ()
  let the-class :: <class> = iclass-class(iclass);
  for (sup :: <class> in direct-superclasses(iclass))
    direct-subclasses(sup) := add-new(direct-subclasses(sup), the-class)
  end for;
  for-each-superclass (super :: <class> of iclass)
    let isuper = class-implementation-class(super);
    for (generic in iclass-subclass-dependent-generics(isuper))
      decache-gf(generic);
    end for;
  end for-each-superclass;
end function;

define function %add-class (class :: <class>) => ()
  record-new-class-subclass-usage(class-implementation-class(class));
end function;

define constant *register-subclass-dependent-generics?* = #t;
define variable *count-tracked-generics?* = #t;
define variable *number-tracked-generics* :: <integer> = 0;

define function %register-subclass-dependent-generic 
    (generic :: <generic-function>, class :: <class>) => ()
  when (*register-subclass-dependent-generics?*)
    let iclass = class-implementation-class(class);
    unless (iclass-subclasses-fixed?(iclass))
      when (*count-tracked-generics?*)
	unless (member?(generic, iclass-subclass-dependent-generics(iclass)))
	  // debug-message("TRACKING %s ON %s", debug-name(generic), debug-name(class));
	  *number-tracked-generics* := *number-tracked-generics* + 1;
	end unless;
      end when;
      iclass-subclass-dependent-generics(iclass)
	:= add-new(iclass-subclass-dependent-generics(iclass), generic);
    end unless;
  end when;
end function;

define function %register-subclasses-dependent-generic
    (generic :: <generic-function>, classes :: <simple-object-vector>) => ()
  do(curry(%register-subclass-dependent-generic, generic), classes)
end function;




define function implementation-class-subtype? (ic1 :: <implementation-class>,
					       ic2 :: <implementation-class>)
 => (answer :: <boolean>)
  block (done)
    let c2 :: <class> = iclass-class(ic2);
    // Don't care if the superclasses are represented as 
    // implementation-classes or class objects.  It's easier to just check 
    // both than to check the type.
    for-each-superclass (x :: <class> of ic1)
      if (x == c2 | x == ic2) done(#t) end; 
    end;
    #f
  end;
end function;



// **** This is algorithmically the same as ^compute-slot-descriptors, q.v.
define method compute-slot-descriptors (impcls :: <implementation-class>, 
					the-supericlasses :: <list>,
					u :: <subjunctive-class-universe>)
 => ()
  let the-class :: <class> = iclass-class(impcls);
  let all-slots :: <stretchy-vector> = make(<stretchy-vector>);
  let icount :: <integer> = 0;
  let ccount :: <integer> = 0;
  let repeater = #f;
  let repeater-size = #f;
  local method collect-superclass-slots (supericlasses :: <list>) => ()
	  unless (empty?(supericlasses))
	    // first get superclass slots
	    collect-superclass-slots(tail(supericlasses));
	    let ic :: <implementation-class> = head(supericlasses);
	    for (sd :: <slot-descriptor> in direct-slot-descriptors(ic))
	      block (duplicate-slot)
		let g = slot-getter(sd);
		for (osd :: <slot-descriptor> in all-slots)
		  if (g == slot-getter(osd)) duplicate-slot() end if
		end for;
		all-slots := add!(all-slots, sd);
		select (slot-allocation(sd) by \==)
		  #"instance" => icount := icount + 1;
		  #"each-subclass", #"class" => ccount := ccount + 1;
		  #"virtual" => ;
		  #"repeated" =>
		    if (repeater)
		      error(make(<simple-slot-error>,
				 format-string: "Multiple repeated slots %= and %= in %=", 
				 format-arguments: list(repeater, sd, the-class)))
		    else
		      repeater := sd;
		      repeater-size := size-slot-descriptor(sd);
		      icount := icount + 1     // one for the size slot
		    end if;
		end select
	      end block
	    end for
	  end unless
	end method;
  collect-superclass-slots(the-supericlasses);
  let ivector :: <simple-object-vector> =
    make(<simple-object-vector>, size: icount, fill: #f);
  let cvector :: <simple-object-vector> =
    make(<simple-object-vector>, size: ccount, fill: #f);
  let first-primary :: false-or(<implementation-class>)
    = begin
	local method loop (l :: <list>)
		if (l == #())
		  #f
		else
		  let ic :: <implementation-class> = head(l);
		  if (class-primary?(ic))
		    ic
		  else
		    loop(tail(l))
		  end if
		end if
	      end method;
	loop(tail(the-supericlasses))
      end;
  when (first-primary)
    let first-primary :: <implementation-class> = first-primary;
    let merge-vectors
      = method (into-vec :: <simple-object-vector>, from-vec :: <simple-object-vector>)
          for (i from 0 below size(from-vec))
            let supsd = vector-element(from-vec, i);
            when (supsd)
              let sd = vector-element(into-vec, i);
              if (sd)
                unless (getter=(supsd, sd))
                  error(make(<simple-slot-error>, 
                             format-string: "Class %= has slot conflict with %= and %=",
                             format-arguments: list(the-class, sd, supsd)))
                end unless
              else
                vector-element(into-vec, i) := supsd
              end if
            end when
          end for
        end method;
    merge-vectors(ivector, instance-slot-descriptors(first-primary));
    merge-vectors(cvector, class-slot-descriptors(first-primary))
  end when;
  local method allocate-superclass-slots (supericlasses :: <list>) => ()
	  unless (empty?(supericlasses))
	    // first allocate slots for superclasses
	    allocate-superclass-slots(tail(supericlasses));
	    let ic :: <implementation-class> = head(supericlasses);
	    unless (first-primary & implementation-class-subtype?(first-primary, ic))
	      let position-slot
		= method (sd, vec :: <simple-object-vector>)
		    let n :: <integer> = size(vec);
		    local method loop (i :: <integer>)
			    if (i == n)
			      error("Bug - ran out of space for %= in %=", sd, the-class)
			    else
			      if (vector-element(vec, i))
				loop(i + 1)
			      else
				vector-element(vec, i) := sd
			      end if
			    end if
			  end method;
		    loop(0)
		  end method;
	      for (sd in direct-slot-descriptors(ic))
		select (slot-allocation(sd) by \==)
		  #"instance"  =>
		    position-slot(sd, ivector);
		  #"each-subclass", #"class" => 
		    position-slot(sd, cvector);
		  #"virtual" => ;
		  #"repeated" => 
		    let i :: <integer> = icount - 1;
		    if (~(vector-element(ivector, i)))
		      vector-element(ivector, i) := repeater-size
		    else
		      error("Bug - canonical slot for repeating size in use already")
		    end if;
		end select
	      end for
	    end unless
	  end unless
	end method;
  allocate-superclass-slots(the-supericlasses);
  impcls.slot-descriptors :=
    if (all-slots = ivector)
      // often, all slots are instance slots
      ivector
    elseif (all-slots = cvector)
      // or maybe they're all class slots
      cvector
    else
      // oh, well -- make a new vector
      as(<simple-object-vector>, all-slots)
    end;
  impcls.instance-slot-descriptors := ivector;
  impcls.class-slot-descriptors := cvector;
  impcls.repeated-slot-descriptor := repeater;
  impcls.instance-storage-size := icount;
  values()
end method compute-slot-descriptors;


define method finalize-inheritance (iclass :: <implementation-class>,
				    all-super-iclasses :: <list>,
				    u :: <subjunctive-class-universe>,
				    subtype-mask :: <integer>)
 => ()
  // We must be type-complete to get here.
  iclass.iclass-type-complete? := #t;
  compute-slot-descriptors(iclass, all-super-iclasses, u);
  iclass.class-slot-storage 
    := make(<simple-object-vector>, size: size(class-slot-descriptors(iclass)));
  compute-defaulted-initialization-arguments(iclass, all-super-iclasses, u);
  let mm-wrapper :: <mm-wrapper> 
    = make-mm-wrapper(iclass, 
		      (iclass.instance-storage-size * 4) + 1, // all traceable
		      7);  // non-vector
  mm-wrapper.mm-wrapper-subtype-mask := subtype-mask;
  iclass.class-mm-wrapper := mm-wrapper;
  iclass.class-complete? := #t;
  iclass-instantiable?(iclass) := (~class-abstract?(iclass)
				     // wank wank
				     & class-rcpl-vector(iclass)[0] == <object>)
end method finalize-inheritance;



define function compute-defaulted-initialization-arguments (iclass :: <implementation-class>,
							    all-super-iclasses :: <list>,
							    u :: <subjunctive-class-universe>)
 => ()
  let keywords = #();
  let required-keywords = #();
  let all-init-value? = #t;
  let init-values = #();
  for (ic :: <implementation-class> in all-super-iclasses)
    for (descriptor in direct-initialization-argument-descriptors(ic))
      let keyword = descriptor.init-keyword;
      if (member?(keyword, required-keywords))
	// don't look at it
      elseif (descriptor.init-keyword-required?)
	required-keywords := pair(keyword, required-keywords);
      elseif (descriptor.init-supplied? & ~member?(keyword, keywords))
	keywords := pair(keyword, keywords);
	if (descriptor.init-value?)
	  init-values := pair(descriptor.init-data, init-values);
	else
	  all-init-value? := #f;
	end;
      end;
    end;
  end;
  let vector-size = size(keywords) * 2;
  iclass.defaulted-initialization-arguments-slot :=
    if (all-init-value?)
      let result :: <simple-object-vector> = make(<simple-object-vector>, size: vector-size);
      for (i from 0 by 2,
	   keyword in keywords,
	   init-value in init-values)
	result[i] := keyword;
	result[i + 1] := init-value;
      end;
      result
    else
      vector-size
    end;
end function compute-defaulted-initialization-arguments;



//// Class Precedence List

define class <inconsistent-precedence-class-error> (<simple-error>) end;

define function compute-implementation-class-precedence-list (c :: <implementation-class>,
							      u :: <subjunctive-class-universe>)
 => cpl :: <list>;
  let convert = scu-converter(u);
  local method merge-lists (partial-cpl :: <list>, remaining-lists :: <list>)
	  // The partial-cpl is in reverse order at this point.
	  if (every?(empty?, remaining-lists))
	    reverse!(partial-cpl)
	  else
	    local 
                  method candidate (s :: <implementation-class>) 
                    local 
                          method tail? (l :: <list>)
                            member?(s, tail(l))
                          end method;

                    ~any?(tail?, remaining-lists) 
                      & s
                  end method,

                  method candidate-at-head (l :: <list>)
                    ~empty?(l) & candidate(head(l))
                  end method;
          
            let next = any?(candidate-at-head, remaining-lists);
	    
	    if (next)
	      local method remove-next (l :: <list>)
		      if (head(l) == next) tail(l) else l end
		    end method;
	      merge-lists(pair (next, partial-cpl),
			  map-into(remaining-lists, remove-next, remaining-lists))
	    else
	      error(make(<inconsistent-precedence-class-error>,
			 format-string: "Inconsistent precedence graph"))
	    end
	  end
	end;
  
  let c-direct-superclasses = map-as(<list>, convert, direct-superclasses(c));
  let c3 = merge-lists(list(c),
                       concatenate(map(rcurry(all-iclass-superclasses, u),
                                       c-direct-superclasses),
                                   list(c-direct-superclasses)));
  let old = compute-implementation-class-precedence-list-old(c, u);
  unless (every?(\=, c3, old))
    signal("The class precedence list of %= differ, Dylan: %=; C3: %=", c, old, c3)
  end;
  c3;
end function compute-implementation-class-precedence-list;


define function compute-implementation-class-precedence-list-old
    (c :: <implementation-class>, u :: <subjunctive-class-universe>)
 => cpl :: <list>;
  let convert = scu-converter(u);
  local method merge-lists (partial-cpl :: <list>, remaining-lists :: <list>)
	  // The partial-cpl is in reverse order at this point.
	  if (every?(empty?, remaining-lists))
	    reverse!(partial-cpl)
	  else
	    local 
	      method unconstrained-class (s)
		let s :: <implementation-class> = scu-entry(s, u);
		local 
		  method s-in-and-unconstrained-in? (l :: <list>)
		    head(l) == s
		  end method,

		  method s-unconstrained-in? (l :: <list>)
		    head(l) == s | ~member?(s, tail(l))
		  end method;

		any?(s-in-and-unconstrained-in?, remaining-lists) 
		  & every?(s-unconstrained-in?, remaining-lists) 
		  & s
	      end method,

	      method unconstrained-class-in-superclasses (c :: <implementation-class>)
		any?(unconstrained-class, direct-superclasses(c))
	      end method;
	    
	    let next = any?(unconstrained-class-in-superclasses, partial-cpl);
	    
	    if (next)
	      local method remove-next (l :: <list>)
		      if (head(l) == next) tail(l) else l end
		    end method;
	      merge-lists(pair (next, partial-cpl),
			  map-into(remaining-lists, remove-next, remaining-lists))
	    else
	      error(make(<inconsistent-precedence-class-error>,
			 format-string: "Inconsistent precedence graph"))
	    end
	  end
	end;
  
  let c-direct-superclasses = map-as(<list>, convert, direct-superclasses(c));
  merge-lists(list(c),
              add(map(rcurry(all-iclass-superclasses, u), c-direct-superclasses),
                  c-direct-superclasses))
  
end function compute-implementation-class-precedence-list-old;



define function compute-cpl-rcpl-stuff (iclass :: <implementation-class>, 
					u :: <subjunctive-class-universe>)
 => (cpl :: <list>, mask :: <integer>)
  let cpl :: <list> = compute-implementation-class-precedence-list(iclass, u);
  iclass.all-superclasses := map(iclass-class, cpl);

  let mask :: <integer>
    = (iterate loop (l :: <list> = tail(cpl), 
		     mask :: <integer> = class-subtype-bit(iclass-class(iclass)),
		     first-primary :: false-or(<implementation-class>) = #f)
	 // => (mask :: <integer>)
	 if (l == #())
	   mask
	 else
	   let sup :: <implementation-class> = head(l);
	   let nxt :: <list> = tail(l);
	   let mask :: <integer> = logior(mask, mm-wrapper-subtype-mask(class-mm-wrapper(sup)));
	   if (class-primary?(sup))
	     if (first-primary
		   & ~subiclass?(first-primary, first-primary.iclass-class, sup, sup.iclass-class)
		   & ~subiclass?(sup, sup.iclass-class, first-primary, first-primary))
	       error("Class %= attempts to combine unrelated primary classes %= and %=.",
		     iclass.iclass-class, first-primary.iclass-class, sup.iclass-class)
	     else
	       loop(nxt, mask, first-primary | sup)
	     end if
	   else
	     loop(nxt, mask, first-primary)
	   end if
	 end if
      end iterate);

  let  self-pos :: <integer> = cpl.size - 1;
  let  rcpl :: <simple-object-vector> = make (<simple-object-vector>, 
					      size: max(self-pos + 1, $min-rcpl-size),
					      fill: #f);
  for (pos :: <integer> from self-pos to 0 by -1, super :: <implementation-class> in cpl)
    rcpl[pos] := iclass-class(super);
  end;
  // now set the slots
  iclass.class-rcpl-vector := rcpl ;
  iclass.class-rcpl-position := self-pos ;
  iclass.class-rcpl-other-positions := #[] ;
  values(cpl, mask)
end;


// Note that when this is called, the implementation class has not been installed
// into the class, which is why we need the universe translation.
// to augment the position data of superclasses needlessly.
define function augment-superior-rcpls (iclass :: <implementation-class>, u :: <subjunctive-class-universe>) => ()
  let the-class :: <class> = iclass-class(iclass);
  for-each-superclass 
      (super :: <class> of iclass, 
         pos :: <integer> from iclass.class-rcpl-position to 1 by -1)
    unless (super == the-class | super == <object>)
      augment-iclass-rcpl-position-data(scu-entry(super, u), pos)
    end
  end
end function;



define generic create-slot-descriptor
    (slot-owner :: <class>, #rest all-keys,
     #key // debug-name, setter, getter, allocation,
     )
 => (sd :: <slot-descriptor>);


define method create-slot-descriptor (class :: <class>,
				      #rest all-keys,
				      #key allocation = #"instance",
				      #all-keys)
 => (sd :: <slot-descriptor>)
  apply(make, allocation.as-slot-descriptor-class, owner: class, all-keys)
end method;


define function check-for-duplicated-slots (slotvec :: <simple-object-vector>) => ()
  // @@@@ Should also check for duplicated setter names, which will cause method overwriting.
  let nslots :: <integer> = size(slotvec);
  local method outer (i :: <integer>, losers :: <list>)
	  if (i == nslots)
	    if (losers ~== #())
	      error(make(<simple-slot-error>,
			 format-string: "Definition of class %= has duplicated slot specs %=",
			 format-arguments: list(slot-owner(head(head(losers))), losers)))
	    end if
	  else
	    let thisslot :: <slot-descriptor> = slotvec[i];
	    local method inner (j :: <integer>, sublosers :: <list>)
		    if (j == nslots)
		      outer(i + 1, if (empty?(sublosers))
				     losers
				   else
				     pair(pair(slot-getter(thisslot), pair(thisslot, sublosers)), losers)
				   end)
		    else
		      let nxtj :: <integer> = j + 1;
		      let thatslot :: <slot-descriptor> = slotvec[j];
		      inner(j + 1,
			    if (i ~== j & getter=(thisslot, thatslot))
			      pair(thatslot, sublosers)
			    else
			      sublosers
			    end if)
		    end if
		  end method;
	    inner(0, #())
	  end if
	end method;
  outer(0, #())
end function;



define function check-for-illegally-inherited-slots (slotvec :: <simple-object-vector>,
						     all-super-iclasses :: <list>)
 => ()
  let losers :: <list> = #();
  let othersups :: <list> = tail(all-super-iclasses);
  for (sd :: <slot-descriptor> in slotvec)
    for (iclass :: <implementation-class> in othersups)
      for (osd :: <slot-descriptor> in direct-slot-descriptors(iclass))
	if (getter=(sd, osd))
	  error(make(<simple-slot-error>,
		     format-string: "The definition of %= respecifies the slot %= from %=.",
		     format-arguments: list(slot-owner(sd),
					    slot-getter(sd),
					    iclass-class(iclass))))
	end if
      end for
    end for
  end for
end function;


define function add-slot-methods (iclass :: <implementation-class>,
				  u :: <subjunctive-class-universe>,
                                  #key override-sealing?)
 => ()
  let slotvec :: <simple-object-vector> = direct-slot-descriptors(iclass);
  for (sd :: <slot-descriptor> in slotvec)
    let g = slot-getter(sd);
    if (g) add-getter-method(slot-owner(sd), g, sd, override-sealing?) end;
    let s = slot-setter(sd);
    if (s) add-setter-method(slot-owner(sd), s, sd, override-sealing?) end;
  end for;
end function;


define function complete-dependent-generic-functions (iclass :: <implementation-class>,
						      u :: <subjunctive-class-universe>)
 => ()
  local method p1 (iclass :: <implementation-class>)
	  do(recompute-type-complete!,  iclass-dependent-generics(iclass));
	  do(compose(p1, rcurry(scu-entry,u)), direct-subclasses(iclass));
	end method;
  p1(iclass);
  local method p2 (iclass :: <implementation-class>)
	  do(rcurry(complete-dependent-generic-function, iclass-class(iclass), u),
	     iclass-dependent-generics(iclass));
	  do(compose(p2, rcurry(scu-entry,u)), direct-subclasses(iclass));
	end method;
  p2(iclass);
end function;
