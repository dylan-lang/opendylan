language: infix-dylan
module: dispatch-engine-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *call-site-caches-enabled?* = #t;

define function call-site-caches-enabled? () => (res)
  *call-site-caches-enabled?*
end function;

define function call-site-caches-enabled?-setter (well?)
  *call-site-caches-enabled?* := well?
end function;

define inline function call-site-caches-possible? 
    (ds :: <dispatch-state>) => (well? :: <boolean>)
  *call-site-caches-enabled?* 
    // & (~*missed-dispatch-in-progress?*
    // 	 | *missed-dispatch-in-progress?* == %ds-gf(ds))
    // & (*missed-dispatch-depth* < 2)
end function;

define variable *partial-dispatch?* = #t;

define open generic partial-dispatch?-setter (value, x);

define method partial-dispatch? (x :: <integer>) => (well?)
  *partial-dispatch?*
end method;

define method partial-dispatch?-setter (value, x :: <integer>)
  *partial-dispatch?* := value;
end method;

define inline function partial-dispatch-possible? (ds :: <dispatch-state>) => (well? :: <boolean>)
  call-site-caches-possible?(ds) & *partial-dispatch?*
end function;

define variable *sharing-partial-dispatch-cache-headers?* = #f;

define inline function sharing-partial-dispatch-cache-headers? () => (well?)
  *sharing-partial-dispatch-cache-headers?*
end function;

define inline function sharing-partial-dispatch-cache-headers?-setter (well?)
  *sharing-partial-dispatch-cache-headers?* := well?;
end function;

define variable *partial-dispatch-megamorphic-punt?* = #f;

define inline function partial-dispatch-megamorphic-punt? () => (well?)
  *partial-dispatch-megamorphic-punt?*
end function;

define inline function partial-dispatch-megamorphic-punt?-setter (well?)
  *partial-dispatch-megamorphic-punt?* := well?;
end function;


//define sealed generic profiling-call-site-cache-header-engine-node-count-1
//    (x :: <profiling-cache-header-engine-node>) => (v :: <raw-machine-word>);

//define sealed generic profiling-call-site-cache-header-engine-node-count-2
//    (x :: <profiling-cache-header-engine-node>) => (v :: <raw-machine-word>);

//define sealed generic profiling-call-site-cache-header-engine-node-count-1-setter
//    (v :: <raw-machine-word>, x :: <profiling-cache-header-engine-node>);

//define sealed generic profiling-call-site-cache-header-engine-node-count-2-setter
//    (v :: <raw-machine-word>, x :: <profiling-cache-header-engine-node>);




define inline-only function stchen-checkedmask 
    (stchen :: <simple-typechecked-cache-header-engine-node>) => (m :: <integer>)
  %load-byte(stchen$v-checkedmask, stchen$s-checkedmask, properties(stchen))
end function;


define function install-cache-header-engine-node-next (old :: <cache-header-engine-node>,
						       next :: type-union(<method>, <engine-node>),
						       gf :: <generic-function>)
 => ()
  // Make sure all side effects to date are to date...
  synchronize-side-effects();
  // Then store.
  cache-header-engine-node-next(old) := next;
  // primitive-enable-cache-header-engine-node(old, gf);
end function;


define constant $cache-header-engine-node-users-increment :: <integer> = 4;


define inline-only function make-new-gf-cache-info-users (current-size :: <integer>)
 => (v :: <simple-object-vector>)
  make(<simple-object-vector>, 
       size: current-size + $cache-header-engine-node-users-increment,
       fill: #f)
end function;



define function track-cache-header-engine-node (e :: <cache-header-engine-node>, c :: <gf-cache-info>)
 => ()
  let v :: <simple-object-vector> = gf-cache-info-users(c);
  let n :: <integer> = size(v);
  local method loop (i :: <integer>)
	  if (i == n)
	    let nv = %make-simple-vector(n + $cache-header-engine-node-users-increment, #f);
	    for (i :: <integer> from 0 below n) vector-element(nv, i) := vector-element(v, i) end;
	    vector-element(nv, n) := e;
	    gf-cache-info-users(c) := nv;
	  else
	    let e2 = vector-element(v, i);
	    if (e2 == #f)
	      vector-element(v, i) := e;
	    elseif (e2 ~== e)
	      loop(i + 1);
	    end if;
	  end if;
	end method;
  loop(0)
end function;


define function compute-headed-methods-under-domain 
    (ds :: <dispatch-state>, dom :: <partial-dispatch-cache-header-engine-node>)
 => ()
  let headed-methods :: <pair> = pair(#f, #());
  let ptr :: <pair> = headed-methods;
  let gf :: <generic-function> = %ds-gf(ds);
  let scu :: <subjunctive-class-universe> = $empty-subjunctive-class-universe;
  for (m :: <method> in generic-function-methods(gf))
    if (~domain-disjoint?(dom, m, scu, gf))
      let nxt :: <pair> = pair(m, #());
      tail(ptr) := nxt;
      ptr := nxt;
    end if;
  end for;
  %ds-headed-methods(ds) := headed-methods;
  let nreq :: <integer> = signature-number-required(function-signature(gf));
  let v :: <simple-object-vector> = %make-simple-vector(nreq, <object>);
  for (i from 0 below nreq) v[i] := domain-type(dom, i) end;
  %ds-argtypes(ds) := v;
end function;



define function compute-typecheckable-argument-mask
  (gf :: <generic-function>, headed-methods :: <pair>) => (bitz :: <integer>);
  if (~*call-site-caches-enabled?* | ~generic-function-sealed?(gf))
    0
  else
    let meths :: <list> = tail(headed-methods);
    let nreq :: <integer> = min(%gf-number-required(gf),
				$simple-typechecked-cache-arguments-limit);
    if (meths == #())
      0
    else
      iterate loop (argnum :: <integer> = 0, bitz :: <integer> = 0)
	if (argnum == nreq)
	  bitz
	else
	  let m :: <method> = head(meths);
	  let next-l :: <list> = tail(meths);
	  let this-spec :: <type> = %method-specializer(m, argnum);
	  if (this-spec == <object>)
	    loop(argnum + 1, bitz)
	  else
	    iterate scan (l :: <list> = next-l)
	      if (l == #())
		loop(argnum + 1, logior(ash(1, argnum), bitz))
	      else
		let m :: <method> = head(l);
		let next-l :: <list> = tail(l);
		if (same-specializer?(this-spec, %method-specializer(m, argnum)))
		  scan(next-l)
		else
		  loop(argnum + 1, bitz)
		end if
	      end if
	    end iterate;
	  end if
	end if
      end iterate
    end if
  end if
end function;


//// Cache Info Creation and Upgrading


define function upgrade-gf-cache-info!
    (new :: <gf-cache-info>, ds :: <dispatch-state>, 
     users :: false-or(<simple-object-vector>))
 => ();
  gf-cache-info-users(new) := users | make-new-gf-cache-info-users(0);
  %ds-cache(ds) := new;
end function;


define function upgrade-to-basic-gf-cache-info (old, ds :: <dispatch-state>)
 => (cache-info :: <gf-cache-info>)
  case
    old == #f | old == 0 => 
      let new :: <gf-cache-info>
	= system-allocate-simple-instance(<gf-cache-info>);
      upgrade-gf-cache-info!(new, ds, #f);
      %gf-cache(%ds-gf(ds)) := new;
    instance?(old, <integer>) =>
      upgrade-to-simple-typechecked-gf-cache-info(old, ds);
    instance?(old, <gf-cache-info>) =>
      old;
    otherwise =>
      error("Bogus gf cache info %=", old);
  end case
end function;


define function upgrade-simple-typechecked-gf-cache-info! 
    (new :: <simple-typechecked-gf-cache-info>, ds :: <dispatch-state>, 
     argmask :: false-or(<integer>),
     entries :: false-or(<simple-object-vector>),
     users :: false-or(<simple-object-vector>))
 => ();
  upgrade-gf-cache-info!(new, ds, users);
  let m :: <integer> = argmask | compute-typecheckable-argument-mask(%ds-gf(ds), %ds-headed-methods(ds));
  simple-typechecked-gf-cache-info-argmask(new) := m;
  simple-typechecked-gf-cache-info-entries(new) 
    := entries | make(<simple-object-vector>, 
		      size: 1 + compress-mask(m, m), // What a crappy way to do logcount.
		      fill: #f);
end function;


define function upgrade-to-simple-typechecked-gf-cache-info (old, ds :: <dispatch-state>)
 => (cache-info :: <simple-typechecked-gf-cache-info>)
  if (instance?(old, <simple-typechecked-gf-cache-info>))
    old
  else
    let new :: <simple-typechecked-gf-cache-info> 
      = system-allocate-simple-instance(<simple-typechecked-gf-cache-info>);
    select (old by instance?)
      <integer>, singleton(#f) => 
	upgrade-simple-typechecked-gf-cache-info!(new, ds, old, #f, #f);
    end select;
    %gf-cache(%ds-gf(ds)) := new
  end if
end function;


// define function upgrade-partial-dispatch-gf-cache-info!
//     (new :: <partial-dispatch-gf-cache-info>, ds :: <dispatch-state>,
//      caches :: false-or(<list>),
//      argmask :: false-or(<integer>),
//      entries :: false-or(<simple-object-vector>),
//      users :: false-or(<simple-object-vector>))
//  => ();
//   upgrade-simple-typechecked-gf-cache-info!(new, ds, argmask, entries, users);
//   partial-dispatch-gf-cache-info-caches(new) := caches | #();
// end function;


// define function upgrade-to-partial-dispatch-gf-cache-info (old-cache-info, ds :: <dispatch-state>)
//  => (cache-info :: <partial-dispatch-gf-cache-info>)
//   if (instance?(old-cache-info, <partial-dispatch-gf-cache-info>))
//     old-cache-info
//   else
//     let new :: <partial-dispatch-gf-cache-info> 
//       = system-allocate-simple-instance(<partial-dispatch-gf-cache-info>);
//     select (old-cache-info by instance?)
//       <simple-typechecked-gf-cache-info> =>
// 	let old :: <simple-typechecked-gf-cache-info> = old-cache-info;
// 	upgrade-partial-dispatch-gf-cache-info!(new, ds, #f,
// 						simple-typechecked-gf-cache-info-argmask(old),
// 						simple-typechecked-gf-cache-info-entries(old),
// 						gf-cache-info-users(old));
//       <integer>, singleton(#f) =>
// 	upgrade-partial-dispatch-gf-cache-info!(new, ds, #f, old-cache-info, #f, #f);
//     end select;
//     %gf-cache(%ds-gf(ds)) := new
//   end if
// end function;
define function compute-argument-precheck-mask (ds :: <dispatch-state>, cache)
 => ();
  let m :: <integer>
    = case
	cache == #f =>
	  // By default, do first the argument positions which have no varying specializers.
	  let gf :: <generic-function> = %ds-gf(ds);
	  %gf-cache(gf) := compute-typecheckable-argument-mask(gf, %ds-headed-methods(ds));
	instance?(cache, <integer>) =>
	  // We've computed it already - maybe at compile time.
	  cache;
	instance?(cache, <simple-typechecked-gf-cache-info>) =>
	  let cache :: <simple-typechecked-gf-cache-info> = cache;
	  simple-typechecked-gf-cache-info-argmask(cache);
	otherwise =>
	  0;
      end case;
  if (m ~== 0) %ds-args-to-check-first(ds) := list(m) end;
end function;

//// Call site cache


define function handle-simple-call-site-cache-head-methods-computed
    (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
 => (root-engine);
  let cache :: <gf-cache-info> = upgrade-to-basic-gf-cache-info(cache, ds);
  track-cache-header-engine-node(old, cache);
  let oengine = cache-header-engine-node-next(old) | $absent-engine-node;
  let nengine = walk-existing-dispatch-engine(ds, oengine, walk-existing-dispatch-engine);
  if (oengine ~== nengine)
    install-cache-header-engine-node-next(old, nengine, %ds-gf(ds));
    subst-engine-node(nengine, oengine, ds);
  end if;
  nengine
end function;

define function handle-simple-call-site-cache-head
    (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
 => (root-engine);
  compute-headed-methods(ds);
  handle-simple-call-site-cache-head-methods-computed(ds, cache, old);
end function;

//// Profiling Header

define function handle-profiling-call-site-cache-head
    (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
 => (root-engine);
  compute-headed-methods(ds);
  handle-simple-call-site-cache-head-methods-computed(ds, cache, old)
end function;

//// Partial Dispatch

define function find-shareable-partial-dispatch-cache-header 
    (old :: <partial-dispatch-cache-header-engine-node>, cache :: <partial-dispatch-gf-cache-info>)
 => (res :: false-or(<partial-dispatch-cache-header-engine-node>))
  if (sharing-partial-dispatch-cache-headers?())
    block (return)
      for (user in gf-cache-info-users(cache))
	if (instance?(user, <partial-dispatch-cache-header-engine-node>))
	  let user :: <partial-dispatch-cache-header-engine-node> = user;
	  if (user == old)
	    return(old)
	  else 
	    block (punt)
	      for (i :: <integer> from 0 below domain-number-required(user))
		unless (same-specializer?(domain-type(user, i), domain-type(old, i)))
		  punt(#f);
		end unless;
	      end for;
	      return(user);
	    end block;
	  end if;
	end if;
      end for;
    end block;
  else 
    #f
  end if
end function;


define function handle-partial-dispatch-cache-head
    (ds :: <dispatch-state>, cache, old :: <partial-dispatch-cache-header-engine-node>)
 => (root-engine);
  let enabled? = partial-dispatch-possible?(ds);
  if (enabled?)
    compute-headed-methods-under-domain(ds, old);
  else 
    compute-headed-methods(ds);
  end if;
  let cache :: <gf-cache-info> = upgrade-to-basic-gf-cache-info(cache, ds);
  // let cache :: <partial-dispatch-gf-cache-info> = upgrade-to-partial-dispatch-gf-cache-info(cache, ds);
  track-cache-header-engine-node(old, cache);
  let oengine = cache-header-engine-node-next(old) | $absent-engine-node;
  let other = oengine == $absent-engine-node & enabled? & find-shareable-partial-dispatch-cache-header(old, cache);
  if (other & other ~== old)
    install-cache-header-engine-node-next(old, other, %ds-gf(ds));
    other
  else 
    let nengine = walk-existing-dispatch-engine(ds, oengine, walk-existing-dispatch-engine);
    if (oengine ~== nengine)
      let current-next = cache-header-engine-node-next(old) | $absent-engine-node;
      if (current-next ~== oengine) // has a punt happened that replugged next?
	current-next
      else 
	install-cache-header-engine-node-next(old, nengine, %ds-gf(ds));
	subst-engine-node(nengine, oengine, ds);
	nengine
      end if
    else 
      nengine
    end if;
  end if
end function;




//// Unknown handler

define patchable-constant handle-unknown-cache-head
  (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
  => ();
  (ds, cache, old)
  begin
    dispwarn(make(<simple-warning>, 
		  format-string: "Unhandled g.f. cache for %= - %=",
		  format-arguments: vector(%ds-gf(ds), old)),
	     ds);
    cache-header-punt(ds, cache, old)
  end
end patchable-constant;


define function find-or-create-common-cache-header 
    (gf :: <generic-function>) => (res :: <common-root-cache-header-engine-node>)
  let d = discriminator(gf);
  if (instance?(d, <common-root-cache-header-engine-node>))
    d
  else 
    let new :: <common-root-cache-header-engine-node>
      = bootstrap-typed-allocate-engine-node(<common-root-cache-header-engine-node>,
					     engine-node$k-cache-header,
					     0);
    primitive-initialize-engine-node(new);
    cache-header-engine-node-parent(new) := gf;
    install-cache-header-engine-node-next(new, d, gf);
    discriminator(gf) := new;
    new
  end if
end function;

define function cache-header-punt (ds :: <dispatch-state>, cache, e :: <cache-header-engine-node>)
 => ()
  let cache :: <gf-cache-info> = upgrade-to-basic-gf-cache-info(cache, ds);
  track-cache-header-engine-node(e, cache);
  let root = find-or-create-common-cache-header(%ds-gf(ds));
  install-cache-header-engine-node-next(e, root, %ds-gf(ds))
end function;



//define function make-simple-typechecked-gf-cache-info (m :: <integer>)
// => (c :: <simple-typechecked-gf-cache-info>);
//  let c :: <simple-typechecked-gf-cache-info>
//    = system-allocate-simple-instance(<simple-typechecked-gf-cache-info>);
//  gf-cache-info-users(c) := make(<simple-object-vector>,
//				 size: $cache-header-engine-node-users-increment,
//				 fill: #f);
//  simple-typechecked-gf-cache-info-argmask(c) := m;
//  simple-typechecked-gf-cache-info-entries(c)
//    := make(<simple-object-vector>, 
//	    size: 1 + compress-mask(m, m), // What a crappy way to do logcount.
//	    fill: #f);
//  c
//end function;


define function handle-simple-typechecked-cache-head 
  (ds :: <dispatch-state>, cache, old :: <simple-typechecked-cache-header-engine-node>)
  => ();
  compute-headed-methods(ds);
  let cache :: <simple-typechecked-gf-cache-info> 
    = upgrade-to-simple-typechecked-gf-cache-info(cache, ds);
  let checkedmask :: <integer> = stchen-checkedmask(old);
  let argmask :: <integer> = simple-typechecked-gf-cache-info-argmask(cache);
  let idx :: <integer> = compress-mask(argmask, checkedmask);
  let entries :: <simple-object-vector> = simple-typechecked-gf-cache-info-entries(cache);
  let nentries :: <integer> = size(entries);
  if (idx + 1 == nentries) track-cache-header-engine-node(old, cache) end;
  let gf :: <generic-function> = %ds-gf(ds);
  install-cache-header-engine-node-next
    (old,
     element(entries, idx)
       | (begin
	    if (~entries[nentries - 1]) 
	      ensure-engine-for-simple-typechecked-gf-cache(checkedmask, ds);
	      if (~entries[nentries - 1]) error("gf caching is hosed") end;
	    end;
	    find-or-install-simple-typechecked-cache
	      (idx, argmask, checkedmask, entries,
	       head(tail(%ds-headed-methods(ds))), gf)
	  end),
     gf)
end function;


define function find-or-install-simple-typechecked-cache (idx :: <integer>,
							  argmask :: <integer>,
							  checkedmask :: <integer>,
							  cachev :: <simple-object-vector>,
							  proto :: <method>,
							  gf :: <generic-function>)
  element(cachev, idx)
    | (element(cachev, idx)
	 := if (argmask == checkedmask)
	      error("Find-or-install-simple-typechecked-cache didn't find all-checked state!")
	    else
	      // Iterate over all the masks which have one more bit set than checkedmask,
	      // in case there is already discrimination engine defined that we can share
	      // with.  If we find one, we use that as the target of a typecheck-discriminator.
	      // If not, pick one, and recurse.
	      let (target, argnum :: <integer>)
		= begin
		    local method loop (m :: <integer>, bitnum :: <integer>, firstargnum)
			    if (m == 0)
			      let firstargnum :: <integer> = firstargnum | error("I'm in trouble!");
			      let ncheckedmask = logior(ash(1, firstargnum), checkedmask);
			      let nidx :: <integer> = compress-mask(argmask, ncheckedmask);
			      values(find-or-install-simple-typechecked-cache
				       (nidx, argmask, ncheckedmask, cachev, proto, gf),
				     firstargnum)
			    elseif (logbit?(0, m) & ~logbit?(bitnum, checkedmask))
			      let e? = element(cachev, compress-mask
						 (argmask, logior(ash(1, bitnum), checkedmask)));
			      if (e?)
				values(e?, bitnum)
			      else
				loop(ash(m, -1), bitnum + 1, firstargnum | bitnum)
			      end if
			    else
			      loop(ash(m, -1), bitnum + 1, firstargnum)
			    end if
			  end method;
		    loop(argmask, 0, #f)
		  end;
	      make-typecheck-discriminator(argnum, gf, %method-specializer(proto, argnum), target)
	    end if)
end function;


define function ensure-engine-for-simple-typechecked-gf-cache 
    (checkedmask :: <integer>, ds :: <dispatch-state>)
  let cache :: <simple-typechecked-gf-cache-info> = %ds-cache(ds);
  let argmask :: <integer> = simple-typechecked-gf-cache-info-argmask(cache);
  let cachevec :: <simple-object-vector> = simple-typechecked-gf-cache-info-entries(cache);
  let nentries :: <integer> = size(cachevec);
  %ds-args-to-check-first(ds)
    := if (argmask == checkedmask)
	 list(argmask)
       else
	 list(checkedmask, logand(argmask, lognot(checkedmask)))
       end if;
  compute-dispatch-from-root(ds, %ds-gf(ds));
  local method lose (desc, datum)
	  let gf :: <generic-function> = %ds-gf(ds);
	  let default = discriminator(gf);
	  for (i from 0 below nentries) if (~cachevec[i]) cachevec[i] := default end; end;
	  dispwarn(make(<simple-warning>, 
			format-string: "For dispatch on %= with args %=, encountered %s %= unexpectedly.",
			format-arguments: vector(gf, reconstruct-args-from-mepargs(gf, %ds-args(ds)), 
						 desc, datum)),
		   ds);
	end method;
  let masque-so-far :: <integer> = 0;
  local method walk (ds :: <dispatch-state>, x, callback :: <function>)
	  let msf :: <integer> = masque-so-far;
	  cachevec[compress-mask(argmask, msf)] := x;
	  // The weird checking order here is to force us to keep iterating until we get
	  // past multiple discriminators which check on the same argument.
	  if (instance?(x, <discriminator>))
	    let x :: <discriminator> = x;
	    let argnum :: <integer> = discriminator-argnum(x);
	    if (argnum < $simple-typechecked-cache-arguments-limit & logbit?(argnum, argmask))
	      if (~logbit?(argnum, msf)) masque-so-far := logior(msf, ash(1, argnum)) end;
	      walk-existing-dispatch-engine(ds, x, callback)
	    elseif (msf ~== argmask)
	      lose("discriminator on arg", argnum);
	      x
	    else
	      x
	    end if
	  else
	    if (msf ~== argmask) lose("non-discriminator", x) end;
	    x
	  end if
	end method;
  walk(ds, discriminator(%ds-gf(ds)), walk)
end function;


define constant $profile-count-low-slot-offset  = 5;
define constant $profile-count-high-slot-offset = 6;

define inline-only function %profile-count-low (di :: <profiling-call-site-cache-header-engine-node>)
 => (low :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (primitive-initialized-slot-value(di, integer-as-raw($profile-count-low-slot-offset))))
end function %profile-count-low;

define inline-only function %profile-count-low-setter
    (new-low :: <machine-word>, di :: <profiling-call-site-cache-header-engine-node>)
  primitive-slot-value(di, integer-as-raw($profile-count-low-slot-offset))
    := primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(new-low));
end function %profile-count-low-setter;

define inline-only function %profile-count-high (di :: <profiling-call-site-cache-header-engine-node>)
 => (high :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (primitive-initialized-slot-value(di, integer-as-raw($profile-count-high-slot-offset))))
end function %profile-count-high;

define inline-only function %profile-count-high-setter
    (new-high :: <machine-word>, di :: <profiling-call-site-cache-header-engine-node>)
  primitive-slot-value(di, integer-as-raw($profile-count-high-slot-offset))
    := primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(new-high));
end function %profile-count-high-setter;

define variable *profile-all-terminal-engine-nodes?* = #f;

define function profile-all-terminal-engine-nodes? () => (well?)
  *profile-all-terminal-engine-nodes?*
end function;

define function profile-all-terminal-engine-nodes?-setter (well?) 
  *profile-all-terminal-engine-nodes?* := well?
end function;



