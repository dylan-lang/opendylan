language: infix-dylan
module: dispatch-engine-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro patchable-constant-definer
  { define patchable-constant ?name:name (?paramlist:*) (?arglist:*) ?body:body end}
    =>
    { define patchable-constant ?name (?paramlist) => (val :: <object>);
          (?arglist)
          ?body
      end}
  { define patchable-constant ?name:name (?paramlist:*) => (?vtypes:*); (?arglist:*) ?body:body end}
    => 
//    { define variable "*" ## ?name ## "*" = #f;
//      define constant ?name = method (?paramlist) => (?vtypes);
//	if ("*" ## ?name ## "*")
//	  "*" ## ?name ## "*" (?arglist)
//	else
//	  ?body
//	end if
//      end method;
//     }
    { define constant ?name = method (?paramlist) => (?vtypes); ?body end method; }
end macro;


define function compute-headed-methods (ds :: <dispatch-state>) 
 => ()
  let headed-methods :: <pair> = pair(#f, #());
  for (m in generic-function-methods(%ds-gf(ds)), ptr :: <pair> = headed-methods then tail(ptr))
    tail(ptr) := pair(m, #())
  end for;
  %ds-headed-methods(ds) := headed-methods;
end function;





define patchable-constant prune-methods-by-known-class
    (argnum :: <integer>, cls :: <class>, ds :: <dispatch-state>)
 => (all-accounted-for? :: <boolean>);
  (argnum, cls, ds)
  let headed-methods :: <pair> = %ds-headed-methods(ds);
  local method loop (prev :: <pair>, subl :: <list>, allall?)
	  if (subl == #())
	    allall?
	  else
	    let meth :: <method> = head(subl);
	    let nxt :: <list> = tail(subl);
	    let (some?, all?) = grounded-has-instances?(cls, %method-specializer(meth, argnum));
	    if (some?)
	      loop(subl, nxt, all? & allall?)
	    else
	      tail(prev) := nxt;
	      loop(prev, nxt, allall?)
	    end if
	  end if
	end method;
  let the-methods :: <list> = tail(headed-methods);
  loop(headed-methods, the-methods, #t)
end patchable-constant;




define function consider-arg-discriminated (ds :: <dispatch-state>, argnum :: <integer>, arg)
 => ()
  local method loop (prev :: <pair>, subl :: <list>)
	  if (subl == #())
	    %ds-add-argnum(argnum, ds)
	  else
	    let meth :: <method> = head(subl);
	    if (grounded-instance?(arg, %method-specializer(meth, argnum)))
	      loop(subl, tail(subl))
	    else
	      let nxt :: <list> = tail(subl);
	      tail(prev) := nxt;
	      loop(prev, nxt)
	    end if
	  end if
	end method;
  let headed-methods :: <pair> = %ds-headed-methods(ds);
  let the-methods :: <list> = tail(headed-methods);
  loop(headed-methods, the-methods)
end function;



define function compute-dispatch-engine (ds :: <dispatch-state>)
 => (new-engine)

  // Now, we have to figure out how to deal with various combinations of
  // cache-header-engine-nodes and caches.

  let cache = %ds-cache(ds);
  let parent :: <dispatch-starter> = %ds-parent(ds);

  if (~*call-site-caches-enabled?*)
    select (parent by instance?)
      <common-root-cache-header-engine-node> =>
	handle-simple-call-site-cache-head(ds, cache, parent);
      <cache-header-engine-node> =>
	cache-header-punt(ds, cache, parent);
      otherwise =>
	handle-standard-dispatch-miss(ds, cache, parent)
    end select
  else
    select (parent by instance?)
      <generic-function> =>
	handle-standard-dispatch-miss(ds, cache, parent);
      <partial-dispatch-cache-header-engine-node> =>
	handle-partial-dispatch-cache-head(ds, cache, parent);
      <simple-typechecked-cache-header-engine-node> =>
	handle-simple-typechecked-cache-head(ds, cache, parent);
      <common-root-cache-header-engine-node> =>
	handle-simple-call-site-cache-head(ds, cache, parent);
      <simple-call-site-cache-header-engine-node> =>
	if (*partial-dispatch?*)
	  cache-header-punt(ds, cache, parent);
	else 
	  handle-simple-call-site-cache-head(ds, cache, parent);
	end if;
      <profiling-call-site-cache-header-engine-node> =>
	handle-profiling-call-site-cache-head(ds, cache, parent);
      <cache-header-engine-node> =>
	handle-unknown-cache-head(ds, cache, parent);
    end select
  end if
end function;



//// Handle ordinary dispatch miss.

define function handle-standard-dispatch-miss (ds :: <dispatch-state>, cache, parent)
 => (root-engine);
  compute-headed-methods(ds);
  compute-argument-precheck-mask(ds, cache);
  let parent :: <dispatch-starter> = %ds-parent(ds);
  compute-dispatch-from-root(ds, parent);
end function;


define function compute-dispatch-from-root (ds :: <dispatch-state>, parent :: <dispatch-starter>)
 => (root-engine)
  let oengine = dispatch-start(parent);
  let nengine = walk-existing-dispatch-engine(ds, oengine, walk-existing-dispatch-engine);
  if (oengine ~== nengine)
    subst-engine-node(nengine, oengine, ds);
    dispatch-start(parent) := nengine;
  end if;
  nengine
end function;



define inline function subst-engine-node (new-engine-node, old-engine-node, ds :: <dispatch-state>)
 => ()
  if (~pointer-id?(new-engine-node, old-engine-node) 
	& old-engine-node ~== $absent-engine-node)
    subst-engine-node-1(new-engine-node, old-engine-node, ds)
  end if
end function;


define macro engine-node-subster
  { engine-node-subster(?new:expression, ?old:expression, ?place:expression) }
    =>
    { begin
	let _new_ = ?new;
	let _old_ = ?old;
	let _v_ = ?place;
	if (pointer-id?(_old_, _v_))
	  ?place := _new_
	else
	  subst-engine-node-2(_new_, _old_, _v_)
	end if
      end
       }
end macro;

define function subst-engine-node-1 (new-e, old-e, ds :: <dispatch-state>) => ()
  let cache = %ds-cache(ds);
  select (cache by instance?)
    <simple-typechecked-gf-cache-info> =>
      // For these guys, we short-circuit walking all the cache-headers by
      // just walking the entries, since all cache header contents should just
      // be pointing to these.  Then we just update the 'next' pointers of the
      // cache-headers in case a top-level entry has changed.
      let cache :: <simple-typechecked-gf-cache-info> = cache;
      let vec :: <simple-object-vector> = simple-typechecked-gf-cache-info-entries(cache);
      for (i :: <integer> from 0 below size(vec))
        engine-node-subster(new-e, old-e, vector-element(vec, i))
      end for;
      for (e :: <cache-header-engine-node> in gf-cache-info-users(cache))
	if (pointer-id?(cache-header-engine-node-next(e), old-e))
	  cache-header-engine-node-next(e) := new-e
	end if
      end for;
    <gf-cache-info> =>
      if (partial-dispatch-megamorphic-punt?()
	    & ~instance?(cache, <common-root-cache-header-engine-node>)
	    // & instance?(cache, <partial-dispatch-gf-cache-info>)
	    & instance?(old-e, <linear-by-class-discriminator>)
	    & instance?(new-e, <hashed-by-class-discriminator>))
	cache-header-punt(ds, cache, %ds-parent(ds));
	#f
      else 
	// By default, walk/replace in all the cache header users of the g.f.
	let cache :: <gf-cache-info> = cache;
	let vec :: <simple-object-vector> = gf-cache-info-users(cache);
	for (i :: <integer> from 0 below size(vec))
	  engine-node-subster(new-e, old-e, vector-element(vec, i));
	end for;
      end if;
    otherwise => #f;
  end select;
end function;


define function subst-engine-node-2 (new-e, old-e, e) => ()
  select (e by instance?)
    <monomorphic-by-class-discriminator> =>
      let e :: <monomorphic-by-class-discriminator> = e;
      engine-node-subster(new-e, old-e, monomorphic-by-class-discriminator-next(e));
    <class-keyed-discriminator> =>
      let e :: <class-keyed-discriminator> = e;
      for (i :: <integer> from 1 below ckd-size(e) by 2)
	engine-node-subster(new-e, old-e, ckd-ref(e, i))
      end for;
      engine-node-subster(new-e, old-e, grounded-class-keyed-discriminator-default(e));
    <if-type-discriminator> =>
      let e :: <if-type-discriminator> = e;
      engine-node-subster(new-e, old-e, if-type-discriminator-then(e));
      engine-node-subster(new-e, old-e, if-type-discriminator-else(e));
    <typecheck-discriminator> =>
      let e :: <typecheck-discriminator> = e;
      engine-node-subster(new-e, old-e, typecheck-discriminator-next(e));
    <singleton-discriminator> =>
      let e :: <singleton-discriminator> = e;
      let tab :: <simple-object-vector> = singleton-discriminator-table(e);
      for (i from 1 below size(tab) by 2)
	engine-node-subster(new-e, old-e, vector-element(tab, i))
      end for;
      engine-node-subster(new-e, old-e, singleton-discriminator-default(e));
    <discriminator> =>
      error("Unhandled discriminator in substitution: %=", e);
    otherwise => #f;
  end select
end function;



//define function find-low-bit-index (m :: <integer>)
//  local method loop (i :: <integer>, m :: <integer>)
//	  if (m == 0) 
//	    error("select-next-arg-for-discrimination got lost?")
//	  elseif (logbit?(0, m))
//	    i
//	  else
//	    loop(i + 1, ash(m, -1))
//	  end if
//	end method;
//  loop(0, m)
//end function;


define patchable-constant select-next-arg-for-discrimination (ds :: <dispatch-state>)
  => (arg);
  (ds)
  begin
    let argnum-set :: <argnum-set> = %ds-argnum-set(ds);
    local method loop ()
	    let a :: <list> = %ds-args-to-check-first(ds);
	    if (a == #())
	      let nrequired :: <integer> = %gf-number-required(%ds-gf(ds));
	      let next-unchecked-arg :: <integer> = next-free-argnum(-1, argnum-set);
	      if (next-unchecked-arg < nrequired)
		next-unchecked-arg
	      else
		#f
	      end if
	    else
	      let m :: <integer> = head(a);
	      local method moop (m :: <integer>, i :: <integer>)
		      if (m == 0)
			let nxt :: <list> = tail(a);
			%ds-args-to-check-first(ds) := nxt;
			loop()
		      elseif (logbit?(0, m) & ~argnum-considered?(i, argnum-set))
			head(a) := ash(m, i);
			i
		      else
			moop(ash(m, -1), i + 1)
		      end if
		    end method;
	      moop(m, 0)
	    end if
	  end method;
    loop()
  end;
end patchable-constant;


define function walk-existing-dispatch-engine (ds :: <dispatch-state>, e, recurse :: <function>)
  => (e :: <object>);
  select (e by instance?)
    <by-class-discriminator> =>
      let e :: <by-class-discriminator> = e;
      // This one is handled a little differently than all the other discriminators, because
      // it doesn't always finish the discrimination step on the given arg position.
      let argnum :: <integer> = discriminator-argnum(e);
      dbg("walk-existing-dispatch-engine, arg %=: %=", argnum, e);
      let thisarg = vector-element(%ds-args(ds), argnum);
      let argclass :: <class> = object-class(thisarg);
      let all-accounted-for? = prune-methods-by-known-class(argnum, argclass, ds);
      let key :: <integer> = object-class-unique-key(thisarg);
      let next-e = ckd-lookup(key, e);
      let new-e = if (all-accounted-for? | ~instance?(next-e, <absent-engine-node>))
		    %ds-add-argnum(argnum, ds);
		    consider-arg-discriminated(ds, argnum, thisarg);
		    recurse(ds, next-e, recurse)
		  elseif (recurse ~== walk-existing-dispatch-engine)
		    error("Hey, you're not supposed to get here!")
		  else
		    compute-subdiscriminator-for-arg(ds, argnum, thisarg, argclass)
		  end if;
      if (next-e ~== new-e)
	dbg("  ... arg %=:  new class key %=", argnum, key);
	subst-engine-node(new-e, next-e, ds);
	ckd-add!(e, key, new-e)
      else e
      end if;
    <discriminator> =>              // Existing discrimination code.
      let e :: <discriminator> = e;
      let argnum :: <integer> = discriminator-argnum(e);
      dbg("walk-existing-dispatch-engine, arg %=: %=", argnum, e);
      let thisarg = vector-element(%ds-args(ds), argnum);
      consider-arg-discriminated(ds, argnum, thisarg);
      select (e by instance?)
	<class-keyed-discriminator> =>
	  let e :: <class-keyed-discriminator> = e;
	  let key = if (instance?(e, <by-class-discriminator>))
		      object-class-unique-key(thisarg)
		    else
		      class-unique-key(thisarg)
		    end if;
	  let next-e = ckd-lookup(key, e);
	  let new-e = recurse(ds, next-e, recurse);
	  if (next-e ~== new-e)
	    dbg("  ... arg %=:  new class key %=", argnum, key);
	    subst-engine-node(new-e, next-e, ds);
	    ckd-add!(e, key, new-e)
	  else e
	  end if;
	<if-type-discriminator> =>
	  let e :: <if-type-discriminator> = e;
	  let testp = primitive-instance?(thisarg, if-type-discriminator-type(e));
	  let next-e = if (testp) if-type-discriminator-then(e) else if-type-discriminator-else(e) end;
	  let new-e = recurse(ds, next-e, recurse);
	  unless (next-e == new-e)
	    subst-engine-node(new-e, next-e, ds);
	    if (testp)
	      dbg("  ... arg %=:  new `then'", argnum);
	      if-type-discriminator-then(e) := new-e
	    else 
	      dbg("  ... arg %=:  new `else'", argnum);
	      if-type-discriminator-else(e) := new-e
	    end if
	  end unless;
	  e;
	<typecheck-discriminator> =>
	  let e :: <typecheck-discriminator> = e;
	  if (primitive-instance?(thisarg, typecheck-discriminator-type(e)))
	    let next-e = typecheck-discriminator-next(e);
	    let new-e = recurse(ds, next-e, recurse);
	    unless (next-e == new-e)
	      dbg("  ... arg %=:  typecheck discriminator extended.", argnum);
	      subst-engine-node(new-e, next-e, ds);
	      typecheck-discriminator-next(e) := new-e
	    end unless;
	    e
	  else 
	    error("Method list incorrectly pruned?")
	  end if;
	<singleton-discriminator> =>
	  let e :: <singleton-discriminator> = e;
	  let key = vector-element(%ds-args(ds), argnum);
	  let lookup = singleton-discriminator-element(e, key, #f);
	  let next-e = lookup | singleton-discriminator-default(e);
	  let new-e = recurse(ds, next-e, recurse);
	  if (next-e ~== new-e) 
	    subst-engine-node(new-e, next-e, ds);
	    if (lookup)
	      dbg("  ... arg %=:  new entry for singleton key %=", argnum, key);
	      singleton-discriminator-element(e, key) := new-e
	    else
	      dbg("  ... arg %=:  new entry for singleton table default %=", argnum);
	      singleton-discriminator-default(e) := new-e
	    end if
	  end if;
	  e;
      end select;
    <cache-header-engine-node> =>
      let e :: <cache-header-engine-node> = e;
      let nxt = cache-header-engine-node-next(e);
      let new = recurse(ds, nxt, recurse);
      if (nxt ~== new)
	subst-engine-node(new, nxt, ds);
	install-cache-header-engine-node-next(new, e, %ds-gf(ds));
      end if;
      e;
    <method>, <terminal-engine-node> =>
      // We are here either because the engine-node is $absent-engine-node -- and we need
      // to fill it in -- or because of a race condition and it's already been filled in --
      // or because something is broken and we are looping or infinitely recursing.
      // Note that once we are in here, there is no more race condition because the g.f.
      // is locked.
      if (recurse ~== walk-existing-dispatch-engine)
	error("Hey, you're not supposed to get here!")
      elseif (e == $absent-engine-node)
	compute-more-dispatch-engine(ds)
      else
	// If there was no multithreading, we shouldn't get here. If there is, 
	// it's possible for another thread to have filled this in ahead of us, so
	// we should just quietly give in.
	e
      end if;
  end select
end function;


define patchable-constant compute-more-dispatch-engine
    (ds :: <dispatch-state>)
 => (engine :: <object> /* union(<method>, <engine-node>) */ );
    (ds)
    begin 
      let argnum? = select-next-arg-for-discrimination(ds);
      if (~ argnum?)
	compute-terminal-engine-node(ds)
      else
	let argnum :: <integer> = argnum?;
	compute-discriminator-for-arg(argnum, ds)
      end if
    end
end patchable-constant;


// This is the interesting part, where we figure out how we must do
// discrimination on the next argument.
define patchable-constant compute-subdiscriminator-for-arg
  (ds :: <dispatch-state>, argnum :: <integer>,
   thisarg, thisargclass :: <class>)
  (ds, argnum, thisarg, thisargclass)
  begin
    dbg("compute-subdiscriminator-for-arg %=", argnum);
    local method ponder-this-arg (methods :: <list>, subclass-p, singletons :: <list>, others :: <list>)
	    if (methods == #())
	      consider-arg-discriminated(ds, argnum, thisarg);
	      let nextd = compute-more-dispatch-engine(ds);
	      let gf :: <generic-function> = %ds-gf(ds);
	      if (subclass-p | (singletons ~== #() & subtype?(thisargclass, <class>)))
		ckd-add!(make-by-singleton-class-discriminator(argnum, gf, 4, $absent-engine-node),
			 class-unique-key(thisarg), nextd)
	      elseif (singletons ~== #())
		let d = make-single-class-singleton-discriminator(singletons, argnum, gf);
		local method poo (l :: <list>)
			if (l == #())
			  dbg("... arg %=:  singletons (default)", argnum);
			  singleton-discriminator-default(d) 
			    := compute-default-subdiscriminator(argnum, gf, thisarg, others, nextd)
			elseif (thisarg == head(l))
			  dbg("... arg %=:  singletons, key = %=", argnum, thisarg);
			  singleton-discriminator-element(d, thisarg) := nextd
			else
			  poo(tail(l))
			end if
		      end method;
		poo(singletons);
		d
	      else
		compute-default-subdiscriminator(argnum, gf, thisarg, others, nextd)
	      end if
	    else
	      let meth :: <method> = head(methods);
	      let spec :: <type> = %method-specializer(meth, argnum);
	      let (subclass-p, singletons :: <list>, others :: <list>)
		= ponder-a-specializer(spec, thisargclass, subclass-p, singletons, others);
	      ponder-this-arg(tail(methods), subclass-p, singletons, others)
	    end if
	  end method;
    let methlist :: <list> = tail(%ds-headed-methods(ds));
    ponder-this-arg(methlist, #f, #(), #())
  end
end patchable-constant;


define patchable-constant ponder-a-specializer
    (spec :: <type>, thisargclass :: <class>, subclass-p, singletons :: <list>, others :: <list>)
 => (subclass-p, singletons :: <list>, others :: <list>);
  (spec, thisargclass, subclass-p, singletons, others)
  select (spec by instance?)
    <class> =>
      values(subclass-p, singletons, others);
    <singleton> => 
      let spec :: <singleton> = spec;
      let obj = spec.singleton-object;
      let singletons :: <list>
	= if (object-class(obj) == thisargclass) pair(obj, singletons) else singletons end;
      values(subclass-p, singletons, others);
    <subclass> =>
      // values(subclass-p | grounded-subtype?(thisargclass, <class>), singletons, others);
      // @@@@@@
      // this would get triggered with limited collection types -- jb 31MAR99
      // debug-assert(grounded-subtype?(thisargclass, <class>),
      // 	      "ponder-a-specializer: %= was expected to be a subtype of <class>", thisargclass);
      values(#t, singletons, others);
    <union> =>
      let spec :: <union> = spec;
      let (subclass-p, singletons :: <list>, others :: <list>)
	= ponder-a-specializer(union-type1(spec), thisargclass, subclass-p, singletons, others);
      ponder-a-specializer(union-type2(spec), thisargclass, subclass-p, singletons, others);
    <limited-type> =>
      values(subclass-p, singletons, 
	     if(grounded-has-instances?(thisargclass, spec)) pair(spec, others) else others end);
  end select
end patchable-constant;


define patchable-constant compute-default-subdiscriminator
    (argnum :: <integer>, gf :: <generic-function>, thisarg, specs :: <list>, nextd)
  (argnum, gf, thisarg, specs, nextd)
  local method subsumed?(t :: <type>, l :: <list>)
	  if (l == #())
	    #f
	  elseif (grounded-subtype?(t, head(l)))
	    #t
	  else
	    subsumed?(t, tail(l))
	  end if
	end method;
  local method foo (specs :: <list>, truelist :: <list>, falselist :: <list>, nextd)
	  if (specs == #())
	    nextd
	  else
	    let spec :: <type> = head(specs);
	    let nextspecs :: <list> = tail(specs);
	    let (thend, elsed) = if (nextd == $absent-engine-node)
				   values(nextd, nextd)
				 elseif (primitive-instance?(thisarg, spec)) 
				   values(nextd, $absent-engine-node)
				 else
				   values($absent-engine-node, nextd)
				 end if;
	    if (subsumed?(spec, truelist))
	      foo(nextspecs, truelist, falselist, thend)
	    elseif (subsumed?(spec, falselist))
	      foo(nextspecs, truelist, falselist, elsed)
	    else
	      make-if-type-discriminator(argnum, gf, spec,
					 foo(nextspecs, pair(spec, truelist), falselist, thend),
					 foo(nextspecs, truelist, pair(spec, falselist), elsed))
	    end if
	  end if
	end method;
  foo(specs, #(), #(), nextd)
end patchable-constant;



define function compute-discriminator-for-arg (argnum :: <integer>, ds :: <dispatch-state>)
 => (engine)
  let thisarg = vector-element(%ds-args(ds), argnum);
  let thisargclass = object-class(thisarg);
  if (tail(%ds-headed-methods(ds)) == #())
    // No specializers (the methods list was empty).
    dispinapplicable(ds)
  else
    let (secondary-p :: <boolean>, force-blowup-p :: <boolean>, 
	 all-subtypes-p :: <boolean>, some-subtypes-p :: <boolean>, 
	 subtype-exception :: false-or(<type>))
      = ponder-this-arg (ds, thisarg, thisargclass, argnum);
    if (~force-blowup-p & all-subtypes-p)
      // No discrimination needed:  the known arg type is a subtype of all specializers.
      consider-arg-discriminated(ds, argnum, thisarg);
      compute-more-dispatch-engine(ds)
    else
      let gf :: <generic-function> = %ds-gf(ds);
      if (force-blowup-p | ~subtype-exception)
	let nextd 
	  = if (secondary-p)
	      prune-methods-by-known-class(argnum, thisargclass, ds);
	      compute-subdiscriminator-for-arg(ds, argnum, thisarg, thisargclass)
	    else
	      consider-arg-discriminated(ds, argnum, thisarg);
	      compute-more-dispatch-engine(ds)
	    end if;
	ckd-add!(make-by-class-discriminator(argnum, gf, 1),
		 class-unique-key(thisargclass), nextd)
      else
	consider-arg-discriminated(ds, argnum, thisarg);
	let nextd = compute-more-dispatch-engine(ds);
	if (some-subtypes-p)
	  // We have to distinguish between one specializer and the rest.
	  let (thend, elsed) = if (primitive-instance?(thisarg, subtype-exception))
				 values(nextd, $absent-engine-node)
			       else
				 values($absent-engine-node, nextd)
			       end if;
	  make-if-type-discriminator(argnum, gf, subtype-exception, thend, elsed)
	else
	  // There is only one specializer, but we don't have type info for it.
	  make-typecheck-discriminator(argnum, gf, subtype-exception, nextd)
	end if
      end if
    end if
  end if
end function;


define function secondary-dispatch-specializer? (spec :: <type>, thisargclass :: <class>)
 => (secondary-p :: <boolean>)
  if (instance?(spec, <union>))
    let spec :: <union> = spec;
    secondary-dispatch-specializer?(spec.union-type1, thisargclass)
      | secondary-dispatch-specializer?(spec.union-type2, thisargclass)
  else
    let (some?, all?) = grounded-has-instances?(thisargclass, spec);
    some? & (~ all?)
  end if
end function;


define function ponder-this-arg (ds :: <dispatch-state>, thisarg, 
				 thisargclass :: <class>, argnum :: <integer>)
 => (secondary-p :: <boolean>, force-blowup-p :: <boolean>, 
     all-subtypes-p :: <boolean>, some-subtypes-p :: <boolean>, 
     subtype-exception :: false-or(<type>))

  let methods :: <list> = tail(%ds-headed-methods(ds));
  let gf :: <generic-function> = %ds-gf(ds);
  let knownargtype :: <type> = %ds-argtype(ds, argnum);
  local method loop (methods :: <list>, secondary-p :: <boolean>, force-blowup-p :: <boolean>,
		     all-subtypes-p :: <boolean>, some-subtypes-p :: <boolean>, subtype-exception)
	 // TODO: TYPIST CAN'T PROVE THIS AND THEN THIS BLOWS TAIL CALLS
	 // => (secondary-p :: <boolean>, force-blowup-p :: <boolean>, 
	 //     all-subtypes-p :: <boolean>, some-subtypes-p :: <boolean>, 
	 //     subtype-exception :: false-or(<type>));
	  if (methods == #())
	    values(secondary-p, force-blowup-p, all-subtypes-p, some-subtypes-p,
		   subtype-exception ~== #t & subtype-exception)
	  else
	    let meth :: <method> = head(methods);
	    let spec :: <type> = %method-specializer(meth, argnum);
	    let secondary-p :: <boolean> 
	      = secondary-p | secondary-dispatch-specializer?(spec, thisargclass);
	    let force-blowup-p :: <boolean>
	      = force-blowup-p | slot-method-requiring-class-discrimination?(meth, argnum);
	    let methods :: <list> = tail(methods);
	    if (concrete-subtype?(knownargtype, spec, gf))
	      loop(methods, secondary-p, force-blowup-p, all-subtypes-p, #t, subtype-exception)
	    else
	      loop(methods, secondary-p, force-blowup-p, #f, some-subtypes-p,
		   if (subtype-exception)
		     if (subtype-exception == #t | ~same-specializer?(subtype-exception, spec))
		       #t
		     else
		       spec
		     end if
		   else
		     spec
		   end if)
	    end if
	  end if
	end method;
  loop(methods, #f, #f, #t, #f, #f)
end function;



define patchable-constant determine-call-keywords (gf :: <generic-function>, methods :: <list>)
  (gf, methods)
  let sig :: <signature> = function-signature(gf);
  if (signature-all-keys?(sig))
    #t
  elseif (signature-key?(sig))
    local method outer (meths :: <list>, ans :: <list>, nans :: <integer>)
	    if (meths == #())
	      let v :: <simple-object-vector> = make(<simple-object-vector>, size: nans);
	      for (i :: <integer> from 0, x in ans) vector-element(v, i) := x end;
	      v
	    else
	      let m :: <method> = head(meths);
	      if (signature-all-keys?(function-signature(m)))
		#t
	      else
		let keys :: <simple-object-vector> = keyword-specifiers(m);
		let n :: <integer> = size(keys);
		local method inner (i :: <integer>, ans :: <list>, nans :: <integer>)
			if (i == n)
			  outer(tail(meths), ans, nans)
			else
			  let k = vector-element(keys, i);
			  if (member?(k, ans))
			    inner(i + 2, ans, nans)
			  else
			    inner(i + 2, pair(k, ans), nans + 1)
			  end if
			end if
		      end method;
		inner(0, ans, nans)
	      end if
	    end if
	  end method;
    outer(methods, #(), 0)
  else 
    #f
  end if
end patchable-constant;

define function compute-terminal-engine-node (ds :: <dispatch-state>)
  => (terminal-thing :: <object> /* union(<method>, <engine-node>) */);
  let methlist :: <list> = tail(%ds-headed-methods(ds));
  let keys = determine-call-keywords(%ds-gf(ds), methlist);
  let (ordered :: <list>, ambig :: <list>) = sort-applicable-methods(methlist, %ds-args(ds));
  dbg("Terminal engine node:  ordered methods = %=, ambig = %=", ordered, ambig);
  let ans = transmogrify-method-list-grounded
    (ds, ordered, ambig, keys,
     *gracefully-dispatch-to-ambiguous-methods*
       | member?(%ds-gf(ds), *permissibly-ambiguous-generics*));
  dbg("Terminal engine node = %=", ans);
  let parent
    = %ds-parent(ds);
  let profiling-parent
    = if (instance?(parent, <cache-header-engine-node>))
	cache-header-engine-node-parent(parent)
      else
	parent
      end if;
  if (*profile-all-terminal-engine-nodes?*
	| instance?(profiling-parent, <profiling-call-site-cache-header-engine-node>))
    let new :: <profiling-call-site-cache-header-engine-node>
      = bootstrap-typed-allocate-engine-node(<profiling-call-site-cache-header-engine-node>,
					     engine-node$k-profiling-cache-header,
					     0);
    primitive-initialize-discriminator(new);
    %profile-count-low(new)  := as(<machine-word>, 0);
    %profile-count-high(new) := as(<machine-word>, 0);
    cache-header-engine-node-parent(new) := parent;
    install-cache-header-engine-node-next(new, ans, %ds-gf(ds));
    new
  else
    ans
  end if
end function;


// Produce an engine-node for a method.
define patchable-constant transmogrify-method-list-grounded 
  (ds :: <dispatch-state>, ordered :: <list>, ambig :: <list>, keyspec, kludge?)
  => (frob :: <object> /* union(<engine-node>, <method>) */);
  (ds, ordered, ambig, keyspec, kludge?)
  begin
    let gf :: <generic-function> = %ds-gf(ds);
    let args :: <simple-object-vector> = %ds-args(ds);
    if (ordered == #())
      if (ambig == #())
	dbg("No applicable methods, punting...");
	dispinapplicable(ds)
      elseif (kludge?)
	dbg("Trying to salvage ambiguous methods");
	let (nordered :: <list>, nambig :: <list>)
	  = sort-applicable-methods-desperately(ambig, args);
	unless (nordered == #() | member?(%ds-gf(ds), *permissibly-ambiguous-generics*))
	  let args = reconstruct-args-from-mepargs(gf, args);
	  dispwarn(make(<ambiguous-methods-warning>,
			generic: gf, arguments: args, ambiguous: ambig,
			format-string: 
			  "Method specificity of unorderable methods %= applying %= to %= "
			  "was determined with arbitrary and capricious rules.",
			format-arguments: vector(ambig, gf, args)),
		   ds)
	end unless;
	transmogrify-method-list-grounded(ds, nordered, nambig, keyspec, #f)
      else
	dbg("No orderable methods, making ambiguous-method engine node.");
	make-ambiguous-methods-engine-node(ordered, ambig)
      end if
    else
      let m :: <method> = head(ordered);
      select (m by instance?)
	<accessor-method> => 
	  let m :: <accessor-method> = m;
	  make-slot-access-engine-node(m, ds);
	otherwise =>
	  let nextp = function-next?(m);
	  let more
	    = if (nextp)
		let moremeths :: <list> = tail(ordered);
		transmogrify-method-list-tail-grounded(ds, ordered, moremeths, ambig, kludge?)
	      else #()
	      end if;
	  if (~more)
	    $absent-engine-node
	  elseif (~nextp & more == #() & (keyspec == #t | keyspec == #f))
	    m
	  else
	    make-single-method-engine-node(m, data: more, keys: keyspec)
	  end if
      end select
    end if
  end
end patchable-constant;

// Recursively generate the next-method chain.
define patchable-constant transmogrify-method-list-tail-grounded
  (ds :: <dispatch-state>, ordered :: <list>, subordered :: <list>,
   ambig :: <list>, kludge? :: <boolean>)
  => (frob :: <list>);
  (ds, ordered, subordered, ambig)
  begin
    if (subordered == #())
      if (ambig == #())
	#()
      elseif (kludge?)
	dbg("Trying to salvage ambiguous method list tail");
	let args :: <simple-object-vector> = %ds-args(ds);
	let (nordered :: <list>, nambig :: <list>)
	  = sort-applicable-methods-desperately(ambig, args);
	unless (nordered == #())
	  let gf = %ds-gf(ds);
	  let args = reconstruct-args-from-mepargs(gf, args);
	  dispwarn(make(<ambiguous-methods-warning>,
			generic: gf, arguments: args, 
			ordered: ordered, ambiguous: ambig,
			format-string: 
			  "Applying %= to %=, ambiguous method ordering of group %= after "
			  "successfully ordered methods %= "
			  "was determined with arbitrary and capricious rules.",
			format-arguments: vector(gf, args, ambig, ordered)),
		   ds)
	end unless;
	transmogrify-method-list-tail-grounded(ds, ordered, nordered, nambig, #f)
      else
	make-ambiguous-methods-next-method(ordered, ambig, %ds-gf(ds))
      end if
    else
      let m :: <method> = head(subordered);
      let more
	= if (function-next?(m))
	    let othermeths :: <list> = tail(subordered);
	    transmogrify-method-list-tail-grounded(ds, ordered, othermeths, 
						   ambig,  kludge?);
	  else
	    #()
	  end if;
      more & select (m by instance?)
	       <accessor-method> => 
		 let m :: <accessor-method> = m;
		 make-slot-accessing-next-method-chain(ds, m);
	       <method>      => pair(m, more);
	     end select
    end if
  end
end patchable-constant;

define patchable-constant %method-applicable?
  (meth :: <method>, args :: <simple-object-vector>) => (answer :: <boolean>);
      (meth, args)
      let n :: <integer> = size(args);
      local method loop (i :: <integer>)
              if (i == n)
                #t
              elseif (primitive-instance?(vector-element(args, i), %method-specializer(meth, i)))
                loop(i + 1)
              else
                #f
              end if
            end method;
      loop(0)
end patchable-constant;

define inline-only function sort-applicable-methods
  (methlist :: <list>, args :: <simple-object-vector>)
      => (ordered-unambiguous :: <list>, unordered-ambiguous :: <list>);
  compute-sorted-applicable-methods-1(methlist, args, %order-methods);
end function;


define inline-only function sort-applicable-methods-desperately
  (methlist :: <list>, args :: <simple-object-vector>)
      => (ordered-unambiguous :: <list>, unordered-ambiguous :: <list>);
  compute-sorted-applicable-methods-1(methlist, args, %order-methods-desperately);
end function;



define patchable-constant compute-sorted-applicable-methods
  (gf :: <generic-function>, args :: <simple-object-vector>)
  => (ordered :: <list>, ambig :: <list>);
  (gf, args)
  let headed-methods :: <pair> = pair(#f, #());
  compute-sorted-applicable-methods-1(for (ans = #() then if (%method-applicable?(m, args)) pair(m, ans) else ans end,
					   m in generic-function-methods(gf))
				      finally ans
				      end for,
				      args, %order-methods)
end patchable-constant;


define patchable-constant compute-sorted-applicable-methods-1
  (methlist :: <list>, args :: <simple-object-vector>, order-the-methods :: <method>)
      => (ordered :: <list>, ambig :: <list>);
      (methlist, args, order-the-methods)
      let ohead :: <pair> = pair(#f, #());
      let ahead :: <pair> = pair(#f, #());
      local method loop (methlist :: <list>) => ()
              unless (methlist == #())
                let meth :: <method> = head(methlist);
		local method make-ambiguous (headed-list :: <pair>) => ();
			local method loop (l :: <list>)
				unless (l == #())
				  let t1 :: <list> = tail(l);
				  tail(l) := tail(ahead);
				  tail(ahead) := l;
				  loop(t1)
				end unless
			      end method;
			loop(tail(headed-list));
			tail(headed-list) := #();
			tail(ahead) := pair(meth, tail(ahead))
		      end method;
		local method precedes-all? (l :: <list>)
			local method loop (l :: <list>)
				if (l == #())
				  #t
				elseif (order-the-methods(meth, head(l), args) ~== #"<")
				  #f
				else
				  loop(tail(l))
				end if
			      end method;
			loop(l)
		      end method;
		local method check-subsequent-ambiguities (oprev :: <pair>) => ();
			if (~precedes-all?(tail(oprev)) | ~precedes-all?(tail(ahead)))
			  make-ambiguous(oprev)
			else
			  tail(oprev) := pair(meth, tail(oprev))
			end if;
		      end method;
		local method insert (oprev :: <pair>, osub :: <list>) => ()
			if (empty?(osub))
			  check-subsequent-ambiguities(oprev)
			else
			  let indic = order-the-methods(meth, head(osub), args);
			  if (indic == #"<")          // Comes before current one.
			    check-subsequent-ambiguities(oprev)
			  elseif (indic == #">")      // Comes after, check further.
			    insert(osub, tail(osub))
			  else        // Ambiguous.  All following ordered methods are too.
			    unless (indic == #"=" & *gracefully-ignore-duplicate-methods*)
			      make-ambiguous(oprev)
			    end unless
			  end if
			end if
		      end method;
		insert(ohead, tail(ohead));
		loop(tail(methlist))
              end unless
            end method;
      loop(methlist);
      values(tail(ohead), tail(ahead))
end patchable-constant;



define variable *gracefully-dispatch-to-ambiguous-methods* = #t;

// See the code in compute-sorted-applicable-methods-1.  What this does is cause
// a duplicated method to get tossed on the floor rather than considered
// ambiguous.  Doing this in any kind of consistent way relies on consistent
// adding of methods to the g.f. - which is (mostly) done (latest addition
// comes first in the generic-function-methods) and on the preservation of this
// ordering through the discrimination pruning (which is also done).
define variable *gracefully-ignore-duplicate-methods* = #t;

define variable *permissibly-ambiguous-generics* :: <list>
  = list(subtype?, As);


define abstract class <ambiguous-methods> (<simple-condition>)
  constant slot ambiguous-methods-generic :: <generic-function>, required-init-keyword: generic:;
  constant slot ambiguous-methods-ordered :: <list>, init-keyword: ordered:, init-value: #();
  constant slot ambiguous-methods-ambiguous :: <list>, required-init-keyword: ambiguous:;
  constant slot ambiguous-methods-arguments :: <sequence>, required-init-keyword: arguments:;
end class;


define class <ambiguous-methods-warning> (<ambiguous-methods>, <warning>)
end class;

define class <ambiguous-methods-error> (<ambiguous-methods>, <error>)
end class;


define patchable-constant %order-methods-desperately (m1 :: <method>, m2 :: <method>, 
						      args :: <simple-object-vector>)
 => (v :: <symbol>);
    (m1, m2, args)
  let nreq :: <integer> = %method-number-required(m1);
  local method loop (idx :: <integer>, ambigp)
	  if (idx == nreq)
	    if (ambigp) #"<>" else #"=" end
	  else
	    let s1 :: <type> = %method-specializer(m1, idx);
	    let s2 :: <type> = %method-specializer(m2, idx);
	    let arg = vector-element(args, idx);
	    let cmp0 :: <symbol> = %order-specializers(s1, s2, arg);
	    let cmp = if (cmp0 == #"<>") %order-specializers-desperately(s1, s2, arg) else cmp0 end;
	    if (cmp == #"<" | cmp == #">")
	      cmp
	    else 
	      loop(idx + 1, ambigp | cmp == #"<>")
	    end if
	  end if
	end method;
  loop(0, #f)
end patchable-constant;


define patchable-constant %order-methods
  (meth1 :: <method>, meth2 :: <method>, args :: <simple-object-vector>)
      => (order :: <symbol>);
      (meth1, meth2, args)
      let nreq :: <integer> = %method-number-required(meth1);
      local method loop (state :: <symbol>, idx :: <integer>)
              if (idx == nreq)
		state
	      else
                let meth1spec :: <type> = %method-specializer(meth1, idx);
                let meth2spec :: <type> = %method-specializer(meth2, idx);
                let cmp :: <symbol> = %order-specializers(meth1spec, meth2spec, 
                                                          vector-element(args, idx));
                let idx :: <integer> = idx + 1;
                if (cmp == #"=")
		  loop(state, idx)
                elseif (cmp ~== #"<>" & (state == #"=" | cmp == state))
		  loop(cmp, idx)
		else
		  #"<>"
		end if
	      end if
	    end method;
      loop (#"=", 0)
end patchable-constant;

define patchable-constant %class<
  (c1 :: <class>, c2 :: <class>, wrt :: <class>) => (o :: <boolean>);
      (c1, c2, wrt)
      /*
      local method fubar (l :: <list>)
              if (empty?(l))
                error("Can't order specializers - arg/reference class %= is neither %= nor %=", wrt, c1, c2)
	      else
		let c :: <class> = head(l);
		let nxt :: <list> = tail(l);
		if (c == c1)
		  #t
		elseif (c == c2)
		  #f
		else fubar(nxt)
		end if
	      end if
            end method;
      fubar(all-superclasses(wrt))
      */
      block (return)
        for-each-superclass (c :: <class> of wrt)
          if (c == c1)
	    return(#t);
	  elseif (c == c2)
            return(#f);
          else
            // look at the next one
          end if;          
        end for-each-superclass;
        error("Can't order specializers - arg/reference class %= is "
              "neither %= nor %=", wrt, c1, c2)          
      end block;
end patchable-constant;


define patchable-constant %order-specializers-default (t1 :: <type>, t2 :: <type>) 
 => (order :: <symbol>, canonical-type :: <type>);
  (t1, t2)
  if (grounded-subtype?(t1, t2))
    values(if (grounded-subtype?(t2, t1)) #"=" else #"<" end, t1)
  elseif (grounded-subtype?(t2, t1))
    values(#">", t2)
  else
    values(#"<>", <object>)
  end if
end patchable-constant;

  
define patchable-constant %order-specializers-desperately (t1 :: <type>, t2 :: <type>, arg)
 => (order :: <symbol>, canonical-type :: <type>);
  (t1, t2, arg)
  if (t1 == t2) 
    values(#"=", t1)
  elseif (instance?(t1, <union>))
    let t1 :: <union> = t1;
    let left :: <type> = union-type1(t1);
    let right :: <type> = union-type2(t1);
    if (primitive-instance?(arg, left))
      if (primitive-instance?(arg, right))
	let (order :: <symbol>, ntype :: <type>) = %order-specializers-desperately(left, right, arg);
	if (order ~== #"<>")
	  %order-specializers-desperately(ntype, t2, arg)
	else
	  values(order, ntype)
	end if
      else
	%order-specializers-desperately(left, t2, arg)
      end if
    elseif (primitive-instance?(arg, right))
      %order-specializers-desperately(right, t2, arg)
    else
      error("You shouldn't get here!")
    end if
  elseif (instance?(t2, <union>))
    let t2 :: <union> = t2;
    let left :: <type> = union-type1(t2);
    let right :: <type> = union-type2(t2);
    if (primitive-instance?(arg, left))
      if (primitive-instance?(arg, right))
	let (order :: <symbol>, ntype :: <type>) = %order-specializers-desperately(left, right, arg);
	if (order ~== #"<>")
	  %order-specializers-desperately(t1, ntype, arg)
	else
	  values(order, ntype)
	end if
      else
	%order-specializers-desperately(t1, left, arg)
      end if
    elseif (primitive-instance?(arg, right))
      %order-specializers-desperately(t1, right, arg)
    else
      error("You shouldn't get here either!")
    end if
  elseif (instance?(t1, <singleton>))
    values(if (instance?(t2, <singleton>)) #"=" else #"<" end, t1)
  elseif (instance?(t2, <singleton>))
    values(#">", t2)
  elseif (instance?(t1, <subclass>))
    let t1 :: <subclass> = t1;
    let c1 :: <class> = t1.subclass-class;
    if (instance?(t2, <subclass>))
      let arg :: <class> = arg;
      let t2 :: <subclass> = t2;
      let c2 :: <class> = t2.subclass-class;
      case 
	(c1 == c2) => values(#"=", t1);
	(%class<(c1, c2, arg)) => values(#"<", t1);
	otherwise => values(#">", t2);
      end case
    elseif (instance?(t2, <class>))
      let c2 :: <class> = t2;
      values(if (subtype?(<class>, c2)) 
	       if (c1 == <object>) %order-specializers(<class>, c2, arg) else #"<" end
	     else #"<>"
	     end,
	     t1)
    else
      %order-specializers-default(t1, t2)
    end if
  elseif (instance?(t2, <subclass>))
    let t2 :: <subclass> = t2;
    if (instance?(t1, <class>))
      let c2 :: <class> = t2.subclass-class;
      let c1 :: <class> = t1;
      values(if (subtype?(<class>, c1))
	       if (c2 == <object>) %order-specializers(c1, <class>, arg) else #">" end if
	     else #"<>"
	     end,
	     t2)
    else
      %order-specializers-default(t1, t2)
    end if
  elseif (instance?(t1, <class>))
    let t1 ::<class> = t1;
    if (instance?(t2, <class>))
      let t2 :: <class> = t2;
      if (%class<(t1, t2, object-class(arg))) values(#"<", t1) else values(#">", t2) end if
    else
      %order-specializers-default(t1, t2)
    end if
  else
    %order-specializers-default(t1, t2)
  end if
end patchable-constant;


define patchable-constant %order-specializers (t1 :: <type>, t2 :: <type>, arg)
 => (order :: <symbol>, canonical-type :: <type>);
  (t1, t2, arg)
  if (t1 == t2) 
    values(#"=", t1)
  elseif (instance?(t1, <singleton>))
    values(if (instance?(t2, <singleton>)) #"=" else #"<" end, t1)
  elseif (instance?(t2, <singleton>))
    values(#">", t2)
  elseif (instance?(t1, <subclass>))
    let t1 :: <subclass> = t1;
    let c1 :: <class> = t1.subclass-class;
    if (instance?(t2, <subclass>))
      let arg :: <class> = arg;
      let t2 :: <subclass> = t2;
      let c2 :: <class> = t2.subclass-class;
      case 
	(c1 == c2) => values(#"=", t1);
	(%class<(c1, c2, arg)) => values(#"<", t1);
	otherwise => values(#">", t2);
      end case
    elseif (instance?(t2, <class>))
      let c2 :: <class> = t2;
      values(if (subtype?(<class>, c2)) 
	       if (c1 == <object>) %order-specializers(<class>, c2, arg) else #"<" end
	     else #"<>"
	     end,
	     t1)
    else
      %order-specializers-default(t1, t2)
    end if
  elseif (instance?(t2, <subclass>))
    let t2 :: <subclass> = t2;
    if (instance?(t1, <class>))
      let c2 :: <class> = t2.subclass-class;
      let c1 :: <class> = t1;
      values(if (subtype?(<class>, c1))
	       if (c2 == <object>) %order-specializers(c1, <class>, arg) else #">" end if
	     else #"<>"
	     end,
	     t2)
    else
      %order-specializers-default(t1, t2)
    end if
  elseif (instance?(t1, <class>))
    let t1 :: <class> = t1;
    if (instance?(t2, <class>))
      let t2 :: <class> = t2;
      if (%class<(t1, t2, object-class(arg))) values(#"<", t1) else values(#">", t2) end if
    else
      %order-specializers-default(t1, t2)
    end if
  else
    %order-specializers-default(t1, t2)
  end if
end patchable-constant;


define patchable-constant same-specializer? (s1 :: <type>, s2 :: <type>) => (answer :: <boolean>);
    (s1, s2)
    select (s1 by instance?)
      <class> =>
	s1 == s2;
      <singleton> => 
	let s1 :: <singleton> = s1;
	if (instance?(s2, <singleton>))
	  let s2 :: <singleton> = s2;
	  singleton-object(s1) == singleton-object(s2)
	else
	  #f
	end if;
      <subclass> =>
	let s1 :: <subclass> = s1;
	if (instance?(s2, <subclass>))
	  let s2 :: <subclass> = s2;
	  s1.subclass-class == s2.subclass-class
	else
	  #f
	end if;
      otherwise =>
	(s1 == s2) | (grounded-subtype?(s1, s2) & grounded-subtype?(s2, s1));
    end select
end patchable-constant;


define inline function same-specializers-spread?
    (req1 :: <simple-object-vector>, nreq1 :: <integer>, 
     req2 :: <simple-object-vector>, nreq2 :: <integer>)
  => (answer :: <boolean>);
  (nreq1 == nreq2
   & (req1 == req2  // in case data sharing makes this easy
	| begin
	    local method loop (i :: <integer>)
		    if (i == nreq1)
		      #t
		    elseif (same-specializer?
			      (vector-element(req1, i),
                               vector-element(req2, i)))
		      loop(i + 1)
		    else
		      #f
		    end if
		  end method;
	    loop(0)
	  end))
end function;

define patchable-constant same-specializers? (sig1 :: <signature>, sig2 :: <signature>)
  => (answer :: <boolean>);
   (sig1, sig2)
  same-specializers-spread?
    (signature-required(sig1), signature-number-required(sig1),
     signature-required(sig2), signature-number-required(sig2))
end patchable-constant;

