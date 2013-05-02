Module:    internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// define open generic %define-domain (gf :: <generic-function>, domain :: <domain>) => ();


define open generic %remove-domain
    (gf :: <generic-function>, domain-spec, in-library :: <library>)
 => (didit? :: <boolean>);


define sealed generic domain-type (d, i :: <integer>) => (t :: <type>);

define sealed generic domain-type-setter (t :: <type>, d, i :: <integer>);

define sealed generic domain-number-required (d) => (n :: <integer>);


// BOOTED:
// define abstract primary class <domain> (<object>) ...
//    slot domain-library :: <library> ...
//    slot domain-next :: false-or(<domain>) ...
//    ...
//
// define primary class <method-domain> (<domain>) ...
//    slot domain-method ...
//    ...
//
// define primary class <standalone-domain> (<domain>)
//    ...
//    repeated slot domain-type ...
//       size-getter: domain-number-required
//    ...


define inline method domain-type (d :: <method-domain>, i :: <integer>) => (t :: <type>)
  %method-specializer(domain-method(d), i)
end method;


define inline method domain-number-required (d :: <method-domain>) => (n :: <integer>)
  %method-number-required(domain-method(d))
end method;


define inline method domain-type (d :: <sequence>, i :: <integer>) => (t :: <type>)
  element(d, i)
end method;

define inline method domain-number-required (d :: <sequence>) => (n :: <integer>)
  size(d)
end method;


define inline method domain-type (d :: <method>, i :: <integer>) => (t :: <type>)
  %method-specializer(d, i)
end method;


define inline method domain-number-required (d :: <method>) => (n :: <integer>)
  %method-number-required(d)
end method;


//define method domain-type (d :: <partial-dispatch-cache-header-engine-node>, i :: <integer>) => (t :: <type>)
//  let m :: <integer> = %load-byte(pdisp$v-typemask, pdisp$s-typemask, properties(d));
//  // I keep being impressed that Dylan doesn't have this.
//  // n.b.  This definition doesn't work on negative numbers and should be done by table lookup.
//  local method logcount (m :: <integer>) => (c :: <integer>)
//          (iterate loop (m :: <integer> = m, c :: <integer> = 0) => (c :: <integer>)
//            if (m == 0) c else loop(ash(m, -1), if (logbit?(0, m)) c + 1 else c end) end
//          end iterate);
//        end method;
//  if (logbit?(i, m))
//    partial-dispatch-type(d, logcount(logand(m, ash(1, i) - 1)))
//  else
//    <object>
//  end if
//end method;

define method domain-type (d :: <partial-dispatch-cache-header-engine-node>, i :: <integer>) => (t :: <type>)
  let m :: <integer> = %load-byte(pdisp$v-typemask, pdisp$s-typemask, properties(d));
  // I keep being impressed that Dylan doesn't have this.
  // n.b.  This definition doesn't work on negative numbers and should be done by table lookup.
  local method logcount (m :: <integer>) => (c :: <integer>)
          local method loop (m :: <integer>, c :: <integer>) => (c :: <integer>)
                  if (m == 0)
                    c
                  else
                    loop(ash(m, -1), if (logbit?(0, m)) c + 1 else c end)
                  end
                end method;
          loop(m, 0)
        end method;
  if (logbit?(i, m))
    partial-dispatch-type(d, logcount(logand(m, ash(1, i) - 1)))
  else
    <object>
  end if
end method;


define method domain-number-required (d :: <partial-dispatch-cache-header-engine-node>)
 => (n :: <integer>)
  signature-number-required(function-signature(parent-gf(d)))
end method;



define sealed generic domain-types (d) => (types :: <simple-object-vector>);


// @@@@ Does this need to be faster?  Copy down over types supported by domain-type.
define method domain-types (d) => (v :: <simple-object-vector>)
  let n :: <integer> = domain-number-required(d);
  let v :: <simple-object-vector> = make(<simple-object-vector>, size: n);
  for (i :: <integer> from 0 below n) vector-element(v, i) := domain-type(d, i) end;
  v
end method;


define inline method domain-types (d :: <method>) => (v :: <simple-object-vector>)
  function-specializers(d)
end method;



define inline-only function library-in? (lib :: <library>, vec :: <simple-object-vector>)
 => (ans :: <boolean>)
  // member?(lib, vec)
  let n :: <integer> = size(vec);
  without-bounds-checks
    iterate loop (i :: <integer> = 0) if (i == n) #f elseif (vec[i] == lib) #t else loop(i + 1) end end;
  end without-bounds-checks;
end function;


define function compute-all-used-libraries (usedvec :: <simple-object-vector>, ans :: <list>)
 => (ans :: <list>)
  let n :: <integer> = size(usedvec);
  local method loop (i :: <integer>, ans :: <list>) => (ans :: <list>)
          if (i == n)
            ans
          else
            let used :: <used-library> = vector-element(usedvec, i);
            let lib :: <library> = used-library(used);
            if (member?(lib, ans))
              loop(i + 1, ans)
            else
              loop(i + 1, compute-all-used-libraries(used-libraries(lib), pair(lib, ans)))
            end if
          end if
        end method;
  loop(0, ans)
end function;


define function library-visible-from? (is-this-visible :: <library>, from-this :: <library>)
 => (well? :: <boolean>)
  is-this-visible == from-this
    |
  begin
    let all-visible :: <simple-object-vector> = all-used-libraries(from-this);
    let all-visible :: <simple-object-vector>
      = if (size(all-visible) ~== 0)
          all-visible
        else
          let used-libs :: <simple-object-vector> = used-libraries(from-this);
          if (size(used-libs) == 0)
            #[]
          else
            let v :: <simple-object-vector>
              = as(<simple-object-vector>, compute-all-used-libraries(used-libs, list(from-this)));
            all-used-libraries(from-this) := v
          end if
        end if;
    library-in?(is-this-visible, all-visible)
  end
end function;


define constant $runtime-library :: <library>
  = make(<library>, name: "the runtime system");

define constant $runtime-module :: <module>
  = make(<module>, name: "phony module", home: $runtime-library);

kludge-up-init-value(<class>, class-module, $runtime-module);


//define function define-domain-on-sealed-generic (g :: <generic-function>, d :: <domain>)
// => (c :: <condition>)
//  let t = domain-types(d);
//  let args = vector(g, t);
//  make(<sealed-generic-function-error>,
//       generic-function: g, operation: %define-domain, arguments: args,
//       format-string: "Defining sealed domain %= %= on sealed function from external library.",
//       format-arguments: args)
//end function;


//define method %define-domain (gf :: <sealed-generic-function>, d :: <domain>) => ()
//  bletch(define-domain-on-sealed-generic(gf, d))
//end method;


define function %define-domain (gf :: <incremental-generic-function>, lib :: <library>, #rest types)
 => ()
  let ntypes :: <integer> = size(types);
  if (~every?(method(x) instance?(x, <type>) end, types))
    error(make(<argument-error>,
               format-string: "One or more types in a domain being defined on %= from library %=,\n"
                 "are not types: %=",
               format-arguments: vector(gf, lib, as(<list>, types))))
  elseif (ntypes ~== function-number-required(gf))
    error(make(<argument-error>,
               format-string: "Incorrect number of types in domain definition on %= from %=;\n"
                 "expected %=, got %=: %=",
               format-arguments: vector(gf, lib, function-number-required(gf), ntypes,
                                        as(<list>, types))))
  else
    let d :: <standalone-domain> = make(<standalone-domain>, library: lib, size: ntypes);
    for (i from 0 below ntypes) domain-type(d, i) := types[i] end;
    %add-domains(gf, vector(d));
  end if
end function;


define function %add-method-domain (gf :: <incremental-generic-function>, m :: <method>, lib :: <library>, checked?)
 => (lossage :: <list>)
  let d :: <method-domain> = make(<method-domain>, next: #f, method: m, library: lib);
  if (checked?)
    (method (#rest v) %add-domains-internal(gf, v) end)(d)
  else
    domain-next(d) := incremental-gf-domain-info(gf);
    incremental-gf-domain-info(gf) := d;
    #()
  end if
end function;


define method %add-domains (gf :: <incremental-generic-function>, domains :: <simple-object-vector>)
 => ()
  let lossage :: <list> = (with-object-lock (gf) %add-domains-internal(gf, domains) end with-object-lock);
  bletch-stack(lossage);
end method;


define function %add-domains-internal (gf :: <incremental-generic-function>,
                                       domains :: <simple-object-vector>)
 => (lossage :: <list>)

  let lossage :: <list> = #();
  for (d :: <domain> in domains)
    let domain-lib :: <library> = domain-library(d);
    if (incremental-gf-sealed?(gf) & ~library-visible-from?(domain-lib, incremental-gf-library(gf)))
      let args = vector(gf, d);
      lossage := add!(lossage,
                      make(<sealed-generic-function-error>,
                           generic-function: gf, operation: %define-domain, arguments: args,
                           format-string:
                             "Defining sealed domain %= %= on sealed generic from external library.",
                           format-arguments: args))
    elseif (type-complete?(gf) & type-complete?(d))
      let gflib :: <library> = incremental-gf-library(gf);
      let libs :: <simple-object-vector> = incremental-gf-method-libraries(gf);
      let nlibs :: <integer> = size(libs);
      let meths :: <list> = #();
      for (m :: <method> in generic-function-methods(gf), i :: <integer> from 0)
        let lib :: <library> = if (i < nlibs) vector-element(libs, i) else gflib end;
        if (~library-visible-from?(lib, domain-lib)
              & ~domain-disjoint?(d, m, $empty-subjunctive-class-universe, #f))
          // If there is a method which falls under the domain, and is defined by
          // a library not visible from the domain's library, we should barf.
          meths := add!(meths, m)
        end if
      end for;
      if (~empty?(meths))
        // This is an error, not a warning, because the method is already there --
        // it's too late to barf about adding the method.
        lossage :=
          add!(lossage,
               make(<domain-sealed-generic-function-error>,
                    generic-function: gf, domain: d, operation: %define-domain,
                    format-string:
                      "Defining sealed domain %= %= conflicts "
                      "with methods %= defined in other libraries.",
                    format-arguments: vector(gf, domain-types(d), meths)))
      else
        %add-domain-unconditionally(gf, d)
      end if
    else
      note-generic-function-incomplete-domain(gf, d)
    end if
  end for;
  lossage
end function;


define method %add-domains (gf :: <sealed-generic-function>, domains) => ()
end method;



define method %add-domain (gf :: <incremental-generic-function>, domain :: <domain>) => ()
  (method(#rest v) %add-domains(gf, v) end)(domain)
end method;


define function %add-nonsiblinged-domain (gf :: <incremental-generic-function>, d :: <domain>) => ()
  (with-object-lock (gf)
     domain-next(d) := incremental-gf-domain-info(gf);
     incremental-gf-domain-info(gf) := d;
  end with-object-lock)
end function;

define function %add-domain-unconditionally
    (g :: <incremental-generic-function>, d :: <domain>) => ()
  let (old :: false-or(<domain>), predecessor :: false-or(<domain>))
    = lookup-domain(d, g);
  if (old)
    let old :: <domain> = old;
    domain-next(d) := domain-next(old)
  end if;
  if (predecessor)
    let predecessor :: <domain> = predecessor;
    domain-next(predecessor) := d
  else
    incremental-gf-domain-info(g) := d
  end if;
end function;



define method %remove-domain (gf :: <incremental-generic-function>,
                              domain-spec, library :: <library>)
 => (didit? :: <boolean>)
  (with-object-lock(gf)
     (begin
        let (old-d :: false-or(<domain>), predecessor :: false-or(<domain>))
          = lookup-domain(domain-spec, gf);
        if (old-d)
          let old-d :: <domain> = old-d;
          let nxt = domain-next(old-d);
          if (predecessor)
            let predecessor :: <domain> = predecessor;
            domain-next(predecessor) := nxt
          else
            incremental-gf-domain-info(gf) := nxt
          end if;
          #t
        else
          #f
        end if
      end)
  end with-object-lock)
end method;



// Subpart for remove-method only.  gf is locked.
define function %remove-method-domain (gf :: <incremental-generic-function>,
                                       m :: <method>, in-lib :: <library>)
 => ()
  local method lookup-domain-1 (link :: false-or(<domain>),
                                 predecessor :: false-or(<domain>))
         => (domain :: false-or(<domain>), predecessor :: false-or(<domain>))
          if (link)
            let link :: <domain> = link;
            if (in-lib == domain-library(link)
                  & instance?(link, <method-domain>)
                  & domain-method(link) == m)
              let nxt = domain-next(link);
              if (predecessor)
                let predecessor :: <domain> = predecessor;
                domain-next(predecessor) := nxt
              else
                incremental-gf-domain-info(gf) := nxt
              end if
            else
              lookup-domain-1(domain-next(link), link)
            end if
          end if
        end method;
  lookup-domain-1(incremental-gf-domain-info(gf), #f)
end function;



define sealed generic domain-match? (d1, d2)
 => (match? :: <boolean>);


define method domain-match? (d1, d2) => (match? :: <boolean>)
  local method loop (i :: <integer>)
          let i :: <integer> = i - 1;
          if (i < 0)
            #t
          elseif (same-specializer?(domain-type(d1, i), domain-type(d2, i)))
            loop(i)
          else
            #f
          end if
        end method;
  loop(domain-number-required(d1))
end method;


define copy-down-method domain-match? (d1 :: <standalone-domain>, d2 :: <standalone-domain>)
  => (match? :: <boolean>);

define copy-down-method domain-match? (d1 :: <standalone-domain>, d2 :: <method-domain>)
  => (match? :: <boolean>);

define method domain-match? (d1 :: <method-domain>, d2 :: <standalone-domain>)
  => (match? :: <boolean>);
  domain-match?(d2, d1)
end method;

define copy-down-method domain-match? (d1 :: <method-domain>, d2 :: <method-domain>)
  => (match? :: <boolean>);

define copy-down-method domain-match? (d1 :: <standalone-domain>, d2 :: <simple-object-vector>)
  => (match? :: <boolean>);

define copy-down-method domain-match? (d1 :: <method-domain>, d2 :: <simple-object-vector>)
  => (match? :: <boolean>);

// engine-node/engine-node domain equivalence:
define copy-down-method domain-match? (d1 :: <partial-dispatch-cache-header-engine-node>,
                                       d2 :: <partial-dispatch-cache-header-engine-node>)
  => (match? :: <boolean>);




define function lookup-domain (d :: <domain>, g :: <incremental-generic-function>)
 => (domain :: false-or(<domain>), predecessor :: false-or(<domain>))
  let in-lib :: <library> = domain-library(d);
  local method lookup-domain-1 (link :: false-or(<domain>),
                                 predecessor :: false-or(<domain>))
         => (domain :: false-or(<domain>), predecessor :: false-or(<domain>))
          if (link)
            let link :: <domain> = link;
            if (in-lib == domain-library(link) & domain-match?(link, d))
              values(link, predecessor)
            else
              lookup-domain-1(domain-next(link), link)
            end if
          else
            values(#f, predecessor)
          end if
        end method;
  lookup-domain-1(incremental-gf-domain-info(g), #f)
end function;



define class <domain-sealed-generic-function-error> (<sealed-generic-function-error>)
  constant slot sealed-generic-function-error-domain :: <sequence>,
    required-init-keyword: domain:;
end class;


define generic domain-conflict? (g :: <generic-function>, frob,
                                 lib :: false-or(<library>), incremental? :: <boolean>, opstring)
 => (ans :: false-or(<condition>));


define method domain-conflict? (g :: <sealed-generic-function>, frob,
                                lib :: false-or(<library>), incremental? :: <boolean>, opstring)
 => (ans :: false-or(<sealed-generic-function-error>))
  ~incremental? & make(<sealed-generic-function-error>,
                       generic-function: g,
                       format-string: "Cannot %s %= in library %= on %= because the generic is sealed.",
                       format-arguments: vector(opstring, frob, lib, g))
end method;


define method domain-conflict? (g :: <incremental-generic-function>, frob,
                                lib :: <library>, incremental? :: <boolean>, opstring)
 => (ans :: false-or(<condition>))
  if (incremental-gf-sealed?(g))
    // Then it's only ok if we are the defining library.
    (lib ~== incremental-gf-library(g) &
       make(<sealed-generic-function-error>,
            generic-function: g,
            format-string: "Cannot %s %= in %= on %= because the generic is sealed.",
            format-arguments: vector(opstring, frob, lib, g)))
  else
    // Otherwise, check to see if the method conflicts with any domains not
    // known about in lib, where it was defined.
    let d :: false-or(<domain>) = incremental-gf-domain-info(g);
    if (d)
      let d :: <domain> = d;
      local method loop (d :: <domain>)
              let dlib :: <library> = domain-library(d);
              // If we're an incremental addition, we permit the method to be added if
              // it's visible from the library where the domain is defined - it's a
              // permissible re- or additional-definition.  Otherwise we only permit
              // the addition for the same library.
              if (if (incremental?) library-visible-from?(lib, domain-library(d)) else lib == dlib end
                    | domain-disjoint?(d, frob, $empty-subjunctive-class-universe, #f))
                let d2 :: false-or(<domain>) = domain-next(d);
                if (d2)
                  let d2 :: <domain> = d2;
                  loop(d2)
                end if
              else
                let dt = domain-types(d);
                make(<domain-sealed-generic-function-error>,
                     generic-function: g, domain: dt,
                     format-string:
                       "Cannot %s %= in %= on %= because "
                       "it is blocked by the sealed domain over %= defined in %=.",
                     format-arguments: vector(opstring, frob, lib, g, dt, dlib))
              end if
            end method;
      loop(d)
    end if
  end if
end method;


// Is the domain not applicable to a set of arguments of the given types?
define sealed generic domain-disjoint?
    (d1, d2, scu :: <subjunctive-class-universe>, dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>);

define function grounded-disjoint-types?
    (t1 :: <type>, t2 :: <type>, scu :: <subjunctive-class-universe>, dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  if (instance?(t1, <class>))
    let t1 :: <class> = t1;
    if (instance?(t2, <class>))
      let t2 :: <class> = t2;
      disjoint-types-1?(t1, t2, scu, dep)
    else
      disjoint-types?(t1, t2, scu, dep)
    end if
  else
    disjoint-types?(t1, t2, scu, dep)
  end if
end function;


define method domain-disjoint? (d1, d2, scu :: <subjunctive-class-universe>, dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  block (return)
    for (i :: <integer> from 0 below domain-number-required(d1))
      if (grounded-disjoint-types?(domain-type(d1, i), domain-type(d2, i), scu, dep))
        return(#t)
      end if
    end for;
    #f
  end
end method;


define copy-down-method domain-disjoint? (d1 :: <standalone-domain>, d2 :: <method>,
                                          scu :: <subjunctive-class-universe>,
                                          dep :: <false-or-dependent-generic-function>)
  => (well? :: <boolean>);

define method domain-disjoint? (d1 :: <method>, d2 :: <standalone-domain>,
                                scu :: <subjunctive-class-universe>,
                                dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  domain-disjoint?(d2, d1, scu, dep)
end method;


define copy-down-method domain-disjoint? (d1 :: <method-domain>, d2 :: <method>,
                                          scu :: <subjunctive-class-universe>,
                                          dep :: <false-or-dependent-generic-function>)
  => (well? :: <boolean>);

define method domain-disjoint? (d1 :: <method>, d2 :: <method-domain>,
                                scu :: <subjunctive-class-universe>,
                                dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  domain-disjoint?(d2, d1, scu, dep)
end method;


define copy-down-method domain-disjoint? (d1 :: <partial-dispatch-cache-header-engine-node>, d2 :: <method>,
                                          scu :: <subjunctive-class-universe>,
                                          dep :: <false-or-dependent-generic-function>)
  => (well? :: <boolean>);

define method domain-disjoint? (d1 :: <method>, d2 :: <partial-dispatch-cache-header-engine-node>,
                                scu :: <subjunctive-class-universe>,
                                dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  domain-disjoint?(d2, d1, scu, dep)
end method;


define copy-down-method domain-disjoint? (d1 :: <simple-object-vector>, d2 :: <method>,
                                          scu :: <subjunctive-class-universe>,
                                          dep :: <false-or-dependent-generic-function>)
  => (well? :: <boolean>);

define method domain-disjoint? (d1 :: <method>, d2 :: <simple-object-vector>,
                                scu :: <subjunctive-class-universe>,
                                dep :: <false-or-dependent-generic-function>)
  => (well? :: <boolean>)
  domain-disjoint?(d2, d1, scu, dep)
end method;



define method type-complete? (d :: <standalone-domain>) => (well? :: <boolean>)
  local method loop (i :: <integer>) => (well? :: <boolean>)
          let i = i - 1;
          if (i < 0)
            #t
          elseif (~type-complete?(domain-type(d, i)))
            #f
          else
            loop(i)
          end if
        end method;
  loop(domain-number-required(d))
end method;


define method recompute-type-complete! (d :: <standalone-domain>) => (well? :: <boolean>)
  type-complete?(d)
end method;

define method map-congruency-classes (f :: <function>, d :: <standalone-domain>) => ()
  for (i from 0 below domain-number-required(d))
    map-congruency-classes(f, domain-type(d, i))
  end for
end method;


define method reduce-incomplete-classes (f :: <function>, d :: <standalone-domain>, ans) => (ans)
  local method loop (i :: <integer>, ans) => (ans)
          let i = i - 1;
          if (i < 0)
            ans
          else
            loop(i, reduce-incomplete-classes(f, domain-type(d, i), ans))
          end if
        end method;
  loop(domain-number-required(d), ans)
end method;


define method type-complete? (d :: <method-domain>) => (well? :: <boolean>)
  type-complete?(domain-method(d))
end method;


define method recompute-type-complete! (d :: <method-domain>) => (well? :: <boolean>)
  recompute-type-complete!(domain-method(d))
end method;


define method map-congruency-classes (f :: <function>, d :: <method-domain>) => ()
  map-congruency-classes(f, domain-method(d))
end method;


define method reduce-incomplete-classes (f :: <function>, d :: <method-domain>, ans) => (ans)
  reduce-incomplete-classes(f, domain-method(d), ans)
end method;
