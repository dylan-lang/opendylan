module: dispatch-profiler
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method function-number-required (f :: <function>) => (res :: <integer>)
  size(function-specializers(f))
end method;

define constant <hit-count> = false-or(<abstract-integer>);

define function as-hit-count (x :: <profiling-call-site-cache-header-engine-node>) => (res :: <hit-count>)
  if (as(<integer>, %profile-count-high(x)) = 0)
    as(<integer>, %profile-count-low(x))
  else
    make(<double-integer>,
	 low:  %profile-count-low(x),
	 high: %profile-count-high(x))
  end if
end function;


define macro incf
  { incf(?x:expression, ?i:expression) }
    => { ?x := generic/+(?x, ?i) }
end macro;

define constant $max-number-parameters = 256;

define constant $shared-argument-types :: <simple-object-vector>
  = make(<simple-object-vector>, size: $max-number-parameters, fill: <object>);


define sealed class <dispatch-walker-state> (<object>)
  slot dws-generic :: <generic-function>;
  slot dws-id :: false-or(<integer>) = #f;
  slot dws-library :: false-or(<library>) = #f;
  slot dws-size :: <integer> = 0;
  constant slot dws-method-weights :: <table> = make(<table>);
  // slot dws-top-profile :: false-or(<abstract-integer>);
  slot dws-arg-types :: <simple-object-vector> = $shared-argument-types;
  slot dws-partial-types :: <simple-object-vector> = $shared-argument-types;
  slot dws-cache-hits :: <integer> = 0;
  slot dws-cache-attempts :: <integer> = 0;
end class;


define sealed class <dws-method-weight> (<object>)
  slot dmw-hits :: <abstract-integer> = 0;
  slot dmw-weighted-hits :: <abstract-integer> = 0;
end class;

define function walk-all-libraries
    (top-lib :: <library>, walk-library :: <function>)
  let walked = make(<table>);
  local method walk-all-libraries*(lib :: <library>, walked :: <object-table>)
	  unless (element(walked, lib, default: #f))
	    walk-library(lib);
	    element(walked, lib) := #t;
	    for (used-library* in used-libraries(lib))
	      walk-all-libraries*(used-library(used-library*), walked)
	    end for;
	  end unless;
	end method;
  walk-all-libraries*(top-lib, make(<table>))
end function;

define function dispatch-walk-all-generics
    (top-lib :: <library>, f :: <function>, generics-walked :: <object-table>)
  walk-all-libraries
    (top-lib, rcurry(dispatch-walk-library-generics, f, generics-walked));
end function;

define function dispatch-walk-library-generics
    (lib :: <library>, f :: <function>, generics-walked :: <object-table>)
  for (g :: <generic-function> in library-defined-generics(lib))
    unless (element(generics-walked, g, default: #f))
      element(generics-walked, g) := #t;
      f(g)
    end unless;
  end for;
end function;

// define function dispatch-walk-all-generic-trees 
//     (top-lib :: <library>, f :: <function>, 
//      generics-walked :: <object-table>, caches-walked :: <object-table>)
//   dispatch-walk-all-generics
//     (top-lib, rcurry(dispatch-walk-generic-tree, f, caches-walked), generics-walked)
// end function;

define function make-type-vector (size :: <integer>) => (res :: <simple-object-vector>)
  make(<simple-object-vector>, size: size, fill: <object>);
end function;

// define function dispatch-walk-generic-tree 
//     (g :: <generic-function>, f :: <function>, caches-walked :: <object-table>)
//   let dws :: <dispatch-walker-state> = make(<dispatch-walker-state>);
//   dws-arg-types(dws)     := make-type-vector(function-number-required(g));
//   dws-partial-types(dws) := make-type-vector(function-number-required(g));
//   dws-generic(dws)       := g;
//   let tree                = discriminator(g);
//   dws-id(dws)            := -1;
//   dws-library(dws)       := #f;
//   dws-size(dws)          := cache-info-size(g);
//   remove-all-keys!(dws-method-weights(dws));
//   dispatch-walker(dws, tree, identity, 0, 0);
//   f(dws);
// end function;

define function dispatch-walk-all-call-sites
    (top-lib :: <library>, f :: <function>, 
     generics-walked :: <object-table>, caches-walked :: <object-table>)
  walk-all-libraries
    (top-lib, rcurry(dispatch-walk-library-call-sites, f, generics-walked, caches-walked));
end function;

define function dispatch-walk-library-call-sites
    (lib :: <library>, f :: <function>, 
     generics-walked :: <object-table>, caches-walked :: <object-table>)
  let dws :: <dispatch-walker-state> = make(<dispatch-walker-state>);
  for (g :: <generic-function> in library-defined-generics(lib))
    unless (element(generics-walked, g, default: #f))
      dws-arg-types(dws)      := make-type-vector(function-number-required(g));
      dws-partial-types(dws)  := make-type-vector(function-number-required(g));
      dws-generic(dws)        := g;

      let tree                 = discriminator(g);
      dws-id(dws)             := -1;
      dws-library(dws)        := #f;
      dws-size(dws)           := cache-info-size(g);
      dws-cache-attempts(dws) := 0;
      dws-cache-hits(dws)     := 0;
      element(caches-walked, tree) := #t;
      remove-all-keys!(dws-method-weights(dws));
      dispatch-walker(dws, tree, identity, 0, 0);
      f(dws);

      let cache = %gf-cache(g);
      when (instance?(cache, <gf-cache-info>))
	for (user in gf-cache-info-users(cache))
	  when (user & ~element(caches-walked, cache-header-engine-node-next(user), default: #f))
	    element(caches-walked, user) := #t;
	    remove-all-keys!(dws-method-weights(dws));
	    let parent = cache-header-engine-node-parent(user);
            let profiling-parent? 
	      = instance?(parent, <profiling-call-site-cache-header-engine-node>);
	    when (profile-all-terminal-engine-nodes?() | profiling-parent?)
	      let (id, library)
		= if (profiling-parent?)
		    values(profiling-call-site-cache-header-engine-node-id(parent),
			   profiling-call-site-cache-header-engine-node-library(parent))
		  else 
		    values(-1, #f) // shared
		  end if;
	      dws-id(dws)             := id;
	      dws-library(dws)        := library;
	      dws-size(dws)           := 0;
	      dws-cache-attempts(dws) := 0;
	      dws-cache-hits(dws)     := 0;
	      dispatch-walker(dws, user, identity, 0, 0);
	      f(dws);
	    end when;
	  end when;
	end for;
      end when;
    end unless;
  end for
end function;


define function dispatch-walk-all-engine-nodes
    (top-lib :: <library>, f :: <function>, 
     generics-walked :: <object-table>, caches-walked :: <object-table>)
  walk-all-libraries
    (top-lib, rcurry(dispatch-walk-library-engine-nodes, f, generics-walked, caches-walked));
end function;


define method clear-dispatch-profiling-counters (e)
  select (e by instance?)
    <profiling-call-site-cache-header-engine-node>
      => let e :: <profiling-call-site-cache-header-engine-node> = e;
         %profile-count-low(e)  := as(<machine-word>, 0);
         %profile-count-high(e) := as(<machine-word>, 0);
    <linear-class-keyed-discriminator>
      => let e :: <linear-class-keyed-discriminator> = e;
         lckd-hits(e) := 0;
    <linear-singleton-discriminator>
      => let e :: <linear-singleton-discriminator> = e;
         lsd-hits(e) := 0;
    otherwise
      => #f;
  end select;
end method;


define function dispatch-walk-library-engine-nodes
    (lib :: <library>, f :: <function>, 
     generics-walked :: <object-table>, caches-walked :: <object-table>)
  let dws :: <dispatch-walker-state> = make(<dispatch-walker-state>);
  for (g :: <generic-function> in library-defined-generics(lib))
    unless (element(generics-walked, g, default: #f))
      let tree  = discriminator(g);
      caches-walked[tree] := #t;
      dws-generic(dws) := g;
      dispatch-walker(dws, tree, f, 0, 0);
      let cache = %gf-cache(g);
      when (instance?(cache, <gf-cache-info>))
	for (user in gf-cache-info-users(cache))
	  when (user)
	    remove-all-keys!(dws-method-weights(dws));
	    let cached-root = cache-header-engine-node-next(user);
	    unless (element(caches-walked, cached-root, default: #f))
	      caches-walked[cached-root] := #t;
	      dispatch-walker(dws, user, f, 0, 0);
	    end unless;		
	  end when;
	end for;
      end when;
    end unless;
  end for;
end function;


define generic dispatch-walker 
    (dws :: <dispatch-walker-state>, object, 
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>);


define macro with-dispatch-walking-type

    { with-dispatch-walking-type (?dws:expression, ?type:expression, ?argnum:expression)
        ?:body
      end } 
 => { let _dws_ = ?dws;
      let _type_ = ?type;
      let _argnum_ = ?argnum;
      let _otype_ = dws-arg-types(_dws_)[_argnum_];
      block ()
	dws-arg-types(_dws_)[_argnum_] := _type_;
	?body;
      afterwards
	dws-arg-types(_dws_)[_argnum_] := _otype_;
      end block }

end macro;

define macro with-preserved-dispatch-walking-types

    { with-preserved-dispatch-walking-types (?dws:expression)
        ?body:body
      end }
 => { let _dws_ = ?dws;
      let _otypes_ = dws-arg-types(_dws_);
      let _ntypes_ = copy-sequence(_otypes_);
      block ()
	dws-arg-types(_dws_) := _ntypes_;
	?body;
      afterwards
	dws-arg-types(_dws_) := _otypes_;
      end block }

end macro;



define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <engine-node>, 
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  0
end method;


define method dispatch-walker 
    (dws :: <dispatch-walker-state>, m :: <method>, 
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  let mwt :: <table> = dws-method-weights(dws);
  let mw :: <dws-method-weight> 
    = element(mwt, m, default: #f) | (mwt[m] := make(<dws-method-weight>));
  if (hits)
    let hits :: <abstract-integer> = hits;
    dmw-hits(mw) := generic/+(dmw-hits(mw), hits);
    dmw-weighted-hits(mw) := generic/+(dmw-weighted-hits(mw), generic/*(hits, cost));
    hits
  else 
    0
  end if;
end method;


define function instance-size (e) => (res :: <integer>)
  let class      = object-class(e);
  let req-size   = size(slot-descriptors(class)) + 1;
  let rep-slotd  = repeated-slot-descriptor(class);
  if (rep-slotd)
    let size-slotd = size-slot-descriptor(rep-slotd);
    let get-size   = slot-getter(size-slotd);
    req-size + get-size(e) 
  else 
    req-size
  end if
end function;

define method dispatch-walker 
    (dws :: <dispatch-walker-state>, e :: <slot-access-engine-node>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  let type = dws-arg-types(dws)[if (instance?(e, <slot-getter-engine-node>)) 0 else 1 end];
  let ic :: <implementation-class>
    = select (type by instance?)
	<singleton>            => object-implementation-class(singleton-object(type));
	<class>                => class-implementation-class(type);
	<implementation-class> => type;
      end select;
  // dws-size(dws) := dws-size(dws) + instance-size(e); // SHARED
  let cic :: <implementation-class>
    = iterate find-any-concrete-subclass (ic :: <implementation-class> = ic)
         if (class-abstract?(ic))
	   block (return)
	     for (sc in direct-subclasses(ic))
	       let csc = find-any-concrete-subclass(class-implementation-class(sc));
	       csc & return(csc);
	     end for;
	     #f
	   end block;
         else	     
	   ic
	 end if
      end iterate;
  let offset :: <integer> = callback-slot-engine-node-offset(e); 
  let sd :: false-or(<slot-descriptor>)
    = select (e by instance?)
	<repeated-slot-access-engine-node> =>
	  repeated-slot-descriptor(cic);
	<instance-slot-engine-node> =>
	  instance-slot-descriptors(cic)[offset];
	<class-slot-engine-node> =>
	  tail(class-slot-storage(cic)[offset]);
      end select;
  let methods
    = generic-function-methods(dws-generic(dws));
  let methood-index
    = find-key(methods,
	       method (m :: <method>)
		 instance?(m, <accessor-method>)
		   & sd == method-slot-descriptor(m)
	       end method);
  if (methood-index)
    dispatch-walker(dws, methods[methood-index], f, cost, hits)
  else 
    0
  end if
end method;


define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <single-method-engine-node>, 
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  dws-size(dws) := dws-size(dws) + instance-size(e); // method and data slots
  dispatch-walker(dws, single-method-engine-node-method(e), f, cost, hits)
end method;


define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <typecheck-discriminator>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  dws-size(dws) := dws-size(dws) + instance-size(e); // type and next slots
  dispatch-walker(dws, typecheck-discriminator-next(e), f, cost + 1, hits)
end method;


define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <if-type-discriminator>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  dws-size(dws) := dws-size(dws) + instance-size(e); // type, then, and else
  let ncost :: <integer> = cost + 1;
  let th = if-type-discriminator-then(e);
  let el = if-type-discriminator-else(e);
  let my-hits :: <abstract-integer> = 0;
  with-dispatch-walking-type (dws, if-type-discriminator-type(e), discriminator-argnum(e))
    incf(my-hits, dispatch-walker(dws, th, f, ncost, #f));
  end;
  incf(my-hits, dispatch-walker(dws, el, f, ncost, #f));
  incf(dws-cache-attempts(dws), my-hits);
  incf(dws-cache-hits(dws), my-hits);
  my-hits
end method;



define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <by-class-discriminator>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  dws-size(dws) := dws-size(dws) + instance-size(e); 
  if (instance?(e, <monomorphic-by-class-discriminator>))
    let k = monomorphic-by-class-discriminator-key(e);
    with-dispatch-walking-type (dws, implementation-class-from-key(k), discriminator-argnum(e))
      let x = monomorphic-by-class-discriminator-next(e);
      let my-hits = dispatch-walker(dws, x, f, cost + 1, #f);
      incf(dws-cache-attempts(dws), my-hits);
      incf(dws-cache-hits(dws), my-hits);
      my-hits
    end
  else    
    let ncost :: <integer> = cost + 1;
    let my-hits :: <abstract-integer> = 0;
    dws-size(dws) := dws-size(dws) + ckd-size(e) + 1; // inlined vector
    for (i from 0 below ckd-size(e) by 2)
      let k = ckd-ref(e, i);
      if (k ~== $ckd-empty)
	with-dispatch-walking-type (dws, implementation-class-from-key(k), discriminator-argnum(e))
	  let x = ckd-ref(e, i + 1);
	  incf(my-hits, dispatch-walker(dws, x, f, ncost, #f))
	end
      end if
    end for;
    incf(dws-cache-attempts(dws), my-hits);
    incf(dws-cache-hits(dws), 
	 if (instance?(e, <linear-class-keyed-discriminator>)) lckd-hits(e) else 0 end if);      
    my-hits
  end if
end method;

// define method dispatch-walker (dws :: <dispatch-walker-state>, e :: <monomorphic-by-class-discriminator>,
// 			       cost :: <integer>, hits :: <hit-count>)
//  => ()
//   dws-size(dws) := dws-size(dws) + 1;
//   let k = monomorphic-by-class-discriminator-key(e);
//   with-dispatch-walking-type (dws, implementation-class-from-key(k), discriminator-argnum(e))
//    let my-hits = dispatch-walker(dws, x, f, cost + 1, #f);
//    incf(dws-cache-attempts(dws), my-hits);
//    incf(dws-cache-hits(dws), my-hits);
//    my-hits
//   end
// end method;


define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <by-singleton-class-discriminator>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  dws-size(dws) := dws-size(dws) + instance-size(e) + ckd-size(e) + 1; 
  let ncost :: <integer> = cost + 1;
  let my-hits :: <abstract-integer> = 0;
  for (i from 0 below ckd-size(e) by 2)
    let k = ckd-ref(e, i);
    if (k ~== $ckd-empty)
      with-dispatch-walking-type (dws, object-implementation-class(implementation-class-from-key(k)),
				  discriminator-argnum(e))
	incf(my-hits, dispatch-walker(dws, ckd-ref(e, i + 1), f, ncost, #f))
      end
    end if
  end for;
  incf(my-hits, dispatch-walker(dws, class-keyed-discriminator-default(e), f, cost + 1, #f));
  incf(dws-cache-attempts(dws), my-hits);
  incf(dws-cache-hits(dws), 
       if (instance?(e, <linear-class-keyed-discriminator>)) lckd-hits(e) else 0 end if);      
  my-hits
end method;



define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <singleton-discriminator>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  let v :: <simple-object-vector> = singleton-discriminator-table(e);
  let ncost :: <integer> = cost + 1;
  let my-hits :: <abstract-integer> = 0;
  dws-size(dws) := dws-size(dws) + instance-size(e) + size(v) + 1; 
  for (i from 0 below size(v) by 2)
    let k = v[i];
    unless (k == $absent-engine-node) // yeah, that's what goes in the key slot (bad idea though).
      incf(my-hits, dispatch-walker(dws, v[i + 1], f, ncost, #f))
    end unless
  end for;
  incf(dws-cache-attempts(dws), my-hits);
  incf(dws-cache-hits(dws), 
       if (instance?(e, <linear-singleton-discriminator>)) lsd-hits(e) else 0 end if);      
  my-hits
end method;




define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <profiling-call-site-cache-header-engine-node>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  dispatch-walker(dws, cache-header-engine-node-next(e), f, cost, as-hit-count(e))
end method;


define method cache-info-size (g :: <generic-function>) => (res :: <integer>)
  if (call-site-caches-enabled?())
    let cache-info = %gf-cache(g);
    if (instance?(cache-info, <gf-cache-info>))
      instance-size(cache-info) + instance-size(gf-cache-info-users(cache-info))
    else 
      0
    end if
  else 
    0
  end if
end method;

define method cache-header-size (e :: <cache-header-engine-node>) => (res :: <integer>)
  instance-size(e)
end method;

define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <cache-header-engine-node>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  unless (debug-name(object-class(e)) == #"<common-root-cache-header-engine-node>")
    dws-size(dws) := dws-size(dws) + cache-header-size(e); 
  end unless;
  dispatch-walker(dws, cache-header-engine-node-next(e), f, cost, hits)
end method;


define method dispatch-walker
    (dws :: <dispatch-walker-state>, e :: <partial-dispatch-cache-header-engine-node>,
     f :: <function>, cost :: <integer>, hits :: <hit-count>)
 => (hits :: <abstract-integer>)
  f(e);
  dws-size(dws) := dws-size(dws) + instance-size(e); // one more for next
  with-preserved-dispatch-walking-types (dws)
    for (i from 0 below domain-number-required(e))
      dws-arg-types(dws)[i] := domain-type(e, i);
      dws-partial-types(dws)[i] := domain-type(e, i)
    end for;
    let next    = cache-header-engine-node-next(e);
    let my-hits = dispatch-walker(dws, next, f, cost, hits);
    when (instance?(next, <profiling-call-site-cache-header-engine-node>))
      // no discrimination, count it as hits
      incf(dws-cache-attempts(dws), my-hits);
      incf(dws-cache-hits(dws),     my-hits);
    end when;
    my-hits
  end;
end method;

