Module:    internal
Synopsis:  Stub interface to dynamic definition used by incremental
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function %define-method
    (gf :: <generic-function>, md :: <method>, lib :: <library>)
 => ()
  %add-a-method(gf, md, lib, #t, #t, #f)
end function;

define function %define-sealed-method
    (gf :: <generic-function>, md :: <method>, lib :: <library>)
 => ()
  %add-a-method(gf, md, lib, #t, #t, #t)
end function;


define variable *incomplete-generic-function-methods* :: <table>
  = make(<table>);


define variable *incomplete-generic-function-domains* :: <table>
  = make(<table>);


define function note-incomplete-method-handler
    (g :: <incremental-generic-function>, m :: <method>, lib :: <library>)
 => ()
  g.incremental-gf-method-complete? := #f;
  *incomplete-generic-function-methods*[g]
    := pair(list(m, lib), element(*incomplete-generic-function-methods*, g, default: #()));
  with-lock ($class-bashing-lock)
    map-congruency-classes(method (c :: <class>) => ()
                             let ic :: <implementation-class> = class-implementation-class(c);
                             iclass-dependent-generics(ic)
                               := add-new!(iclass-dependent-generics(ic), g)
                           end method,
                           m);
  end with-lock;
end function;


define function remove-incomplete-method-handler
    (g :: <incremental-generic-function>, frob, lib :: <library>, test :: <function>)
 => (m :: false-or(<method>))
  let old :: <list> = element(*incomplete-generic-function-methods*, g, default: #());
  unless (empty?(old))
    block (return)
      for (e :: <list> in old)
        let m :: <method> = first(e);
        if (test(m, frob))
          *incomplete-generic-function-methods*[g] := remove!(old, e);
          return(m)
        end if
      end for
    end block
  end unless;
end function;


define inline-only function generic-function-incomplete-methods (g :: <incremental-generic-function>)
 => (stuff :: <list>)
  element(*incomplete-generic-function-methods*, g, default: #())
end function;


define function note-incomplete-domain-handler
    (g :: <incremental-generic-function>, d :: <domain>)
 => ()
  g.incremental-gf-method-complete? := #f;
  *incomplete-generic-function-domains*[g]
    := pair(d, element(*incomplete-generic-function-domains*, g, default: #()));
  with-lock ($class-bashing-lock)
    map-congruency-classes(method (c :: <class>) => ()
                             let ic :: <implementation-class> = class-implementation-class(c);
                             iclass-dependent-generics(ic)
                               := add-new!(iclass-dependent-generics(ic), g)
                           end method,
                           d);
  end with-lock
end function;


define inline-only function generic-function-incomplete-domains (g :: <incremental-generic-function>)
 => (stuff :: <list>)
  element(*incomplete-generic-function-domains*, g, default: #())
end function;



define function remove-incomplete-domain-handler
    (g :: <incremental-generic-function>, frob, lib :: <library>)
 => (m :: false-or(<domain>))
  let old :: <list> = element(*incomplete-generic-function-domains*, g, default: #());
  unless (empty?(old))
    block (return)
      for (d :: <domain> in old)
        if (domain-match?(d, frob))
          *incomplete-generic-function-methods*[g] := remove!(old, d);
          return(d)
        end if
      end for
    end block
  end unless;
end function;




define inline-only function do-incomplete-frobs (f :: <function>, g :: <generic-function>, t :: <table>)
 => ();
  let lossage :: <list>
    = (with-object-lock (g)
         let subl :: <list> = element(t, g, default: #());
         local method inner-loop (subl :: <list>, lossage :: <list>,
                                  hdr :: false-or(<pair>), prev :: <list>)
                 if (subl == #())
                   if (hdr) t[g] := hdr else remove-key!(t, g) end;
                   lossage
                 else
                   let elt = head(subl);
                   let nxt :: <list> = tail(subl);
                   let (win?, nlossage) = f(elt);
                   let (hdr :: false-or(<pair>), prev :: <list>)
                     = if (~win?)
                         let cell :: <pair> = list(elt);
                         values((if (hdr) tail(prev) := cell; hdr else cell end), cell)
                       else
                         values(hdr, prev)
                       end if;
                   let lossage = if (nlossage & nlossage ~== #())
                                   concatenate(lossage, nlossage)
                                 else
                                   lossage
                                 end;
                   inner-loop(nxt, lossage, hdr, prev)
                 end if
               end method;
         inner-loop(subl, #(), #f, subl)
       end with-object-lock);
  bletch-stack(lossage);
end function;


begin
  note-generic-function-incomplete-method
    := note-incomplete-method-handler;
  note-generic-function-incomplete-domain
    := note-incomplete-domain-handler;
  remove-generic-function-incomplete-method
    := remove-incomplete-method-handler;
  remove-generic-function-incomplete-domain
    := remove-incomplete-domain-handler;
end;


//// COMPLETENESS DEFERRED ADDITIONS



define thread variable *generics-being-finished* = #f;


// define macro with-batch-deferred-generic-handling
//   { with-batch-deferred-generic-handling () ?body:body end }
//     =>
//     { (begin
//          let %old-value% = *generics-being-finished*;
//          block ()
//            unless (%old-value%) *generics-being-finished* := #t end;
//            ?body
//          cleanup
//            unless (%old-value%)
//              let z = *generics-being-finished*;
//              *generics-being-finished* := #f;
//              finish-the-generics(z)
//            end
//          end
//        end)
//        }
// end macro;


define method note-type-completeness-change (g :: <generic-function>) => ()
  if (instance?(g, <incremental-generic-function>))
    let z = *generics-being-finished*;
    if (~z)
      handle-generic-completeness-change(g)
    else
      let z :: <table> = (if (z == #t) *generics-being-finished* := make(<table>) else z end);
      z[g] := #t
    end if
  end if
end method;


// define function finish-the-generics (z) => ()
//   if (z ~== #t)
//     let z :: <table> = z;
//     for (ignore keyed-by g :: <incremental-generic-function> in z)
//       unless (type-complete?(g)) recompute-type-complete!(g) end
//     end for;
//     for (ignore keyed-by g :: <incremental-generic-function> in z)
//       // @@@@ Insert a restart here to handle aborting deferred additions to this gf
//       handle-generic-completeness-change(g)
//     end for
//   end if
// end function;


define method handle-generic-completeness-change (g :: <generic-function>)
  if (instance?(g, <incremental-generic-function>) & recompute-type-complete!(g))
    let t = *incomplete-generic-function-domains*;
    let g :: <incremental-generic-function> = g;
    if (t)
      let t :: <table> = t;
      do-incomplete-frobs
        (method (d :: <domain>)
           if (recompute-type-complete!(d))
             values(#t, %add-domains-internal(g, vector(d)))
           else
             values(#f, #())
           end if
         end method,
         g, t)
    end if;
    let t = *incomplete-generic-function-methods*;
    if (t)
      let t :: <table> = t;
      do-incomplete-frobs
        (method (e :: <list>)
           let m :: <method> = first(e);
           if (recompute-type-complete!(m))
             let lib :: <library> = second(e);
             values(#t, add-method-internal(g, m, lib, #t, #t))
           else
             values(#f, #())
           end if
         end method,
         g, t)
    end if
  end if
end method;



define method complete-dependent-generic-function (g :: <incremental-generic-function>,
                                                   c :: <class>,
                                                   u :: <subjunctive-class-universe>)
 => ()
  /*
  if (type-complete?(g) & ~incremental-gf-method-complete?(g))
    (with-object-lock (g)
       block ()
         for (d :: <domain> in generic-function-incomplete-domains(g))
           if (type-complete?(d))
             generic-function-incomplete-domains(g)
               := remove(generic-function-incomplete-domains(g), d);
             block
                 let urk = %add-domain-internal(g, d, #t);
                 if (urk) bletch(urk) end;
             exception(<abort>,
                         init-arguments: vector(format-string: "Skip adding domain %= to %=.",
                                                  format-arguments: vector(d, g)))
             end block
           end if
         end for;
         for (e :: <list> in generic-function-incomplete-methods(g))
           let m :: <method> = first(e);
           let lib :: <library> = second(e);
           if (type-complete?(m))
             generic-function-incomplete-methods(g)
               := remove(generic-function-incomplete-methods(g), e);
             block
                 let urk = add-method-internal(g, m, lib, #t, #t);
                 if (urk) bletch(urk) end;
             exception(<abort>,
                         init-arguments: vector(format-string: "Skip adding method %= to %=.",
                                                  format-arguments: vector(m, g)))
             end block
           end if
         end for
       end block
    end with-object-lock)
  end if
  */
  note-type-completeness-change(g)
end method;


//// Generic function definition/redefinition

define inline function %%define-generic
    (gf :: <generic-function>,
     debug-name, module, sealed?, complain-about-congruency?, signature)
 => (gf :: <generic-function>);
  let lossage :: <list> = #();
  (with-object-lock (gf)
     (begin
        let methods = generic-function-methods(gf);
        let (mlibs :: <simple-object-vector>, glib :: <library>)
          = if (instance?(gf, <incremental-generic-function>))
              let gf :: <incremental-generic-function> = gf;
              values(incremental-gf-method-libraries(gf), incremental-gf-library(gf))
            else
              values(#[], $runtime-library)
            end if;
        let nlibs :: <integer> = size(mlibs);
        reinitialize(gf, debug-name: debug-name, signature: signature,
                     sealed?: sealed?, module: module);
        for (m :: <method> in methods, i :: <integer> from 0)
          let (well?, reason) = congruent?(gf, m);
          if (well?)
            let (_ans, barfo) = add-method-internal(gf, m, if (i < nlibs) mlibs[i] else glib end, #f, #f);
            if (barfo) bletch(barfo) end;
          elseif (complain-about-congruency?)
            lossage := pair(list(m, reason), lossage)
          end if
        end for
      end)
  end with-object-lock);
  if (lossage ~== #())
    error(make(<argument-error>,
               format-string: "Generic function definition for %= was not congruent with some existing methods %=.",
               format-arguments: list(gf, lossage)))
  end if;
  handle-generic-completeness-change(gf);
  gf
end function;

define function %define-generic
    (gf :: <incremental-generic-function>, signature)
 => (gf :: <generic-function>);
  %%define-generic
    (gf, debug-name(gf), incremental-gf-module(gf),
     incremental-gf-sealed?(gf), #t, signature)
end function;

define function %redefine-generic
    (gf :: <incremental-generic-function>, debug-name, module, signature, sealed?)
 => (gf :: <generic-function>);
  %%define-generic
    (gf, debug-name, module, sealed?, #f, signature)
end function;


//// Class definition/redefinition


define function %define-class
    (class :: <class>, superclasses, slots)
 => (class :: <class>);
  reinitialize
    (class,
     debug-name:      debug-name(class),
     module:          class-module(class),
     superclasses:    superclasses,
     abstract?:       class-abstract?(class),
     primary?:        class-primary?(class),
     sealed?:         class-sealed?(class),
     slots:           slots,
     inherited-slots: #[],
     keywords:        #[],
     dependent-generics:
       iclass-dependent-generics(class-implementation-class(class)));
  class
end function;

define function %define-complex-class
    (class :: <class>, superclasses, slots, inherited-slots, keywords)
 => (class :: <class>);
  reinitialize
    (class,
     debug-name:      debug-name(class),
     module:          class-module(class),
     superclasses:    superclasses,
     abstract?:       class-abstract?(class),
     primary?:        class-primary?(class),
     sealed?:         class-sealed?(class),
     slots:           slots,
     inherited-slots: inherited-slots,
     keywords:        keywords,
     dependent-generics:
       iclass-dependent-generics(class-implementation-class(class)));
  class
end function;


define function dependent-subclasses (classes :: <list>, ans :: <list>) => (ans :: <list>)
  iterate loop (classes :: <list> = classes, ans :: <list> = ans)
    if (empty?(classes))
      ans
    else
      let the-head :: <class> = head(classes);
      let the-tail :: <list> = tail(classes);
      if (the-head == <object>)
        ans
      elseif (member?(the-head, ans))
        // If a class has already been done, then its subclasses have been or
        // are in the process of being done.
        loop(the-tail, ans)
      else
        loop(the-tail, dependent-subclasses(direct-subclasses(the-head),
                                            pair(the-head, ans)))
      end if
    end if
  end iterate
end function;


define function %%redefine-complex-class
    (class :: <class>,
     debug-name, module, abstract?, primary?, sealed?,
     superclasses, slots, inherited-slots, keywords)
 => (class :: <class>);
  let superclasses :: <sequence> = if (instance?(superclasses, <sequence>))
                                     superclasses
                                   else
                                     vector(superclasses)
                                   end if;
  let slots :: <simple-object-vector>
    = map-as(<simple-object-vector>,
             method (stuff) apply(create-slot-descriptor, class, stuff) end,
             slots);
  let ans =
    with-lock ($class-bashing-lock)
      let dependents :: <list> = dependent-subclasses(direct-subclasses(class), list(class));
      if (nonstructural-redefinition?(class, superclasses, slots))
        %redefine-class-attributes-only
          (class, dependents,
           superclasses:    superclasses,
           slots:           slots,
           inherited-slots: inherited-slots,
           keywords:        keywords);
      else
        %redefine-class-of-new-structure
          (class, dependents,
           debug-name:      debug-name,
           superclasses:    superclasses,
           abstract?:       abstract?,
           primary?:        primary?,
           sealed?:         sealed?,
           module:          module,
           slots:           slots,
           inherited-slots: inherited-slots,
           keywords:        keywords);
      end if
    end with-lock;
  if (instance?(ans, <condition>))
    bletch(ans)
  end;
  class
end function;

define function %redefine-class
    (class :: <class>,
     debug-name, module, abstract?, primary?, sealed?,
     superclasses, slots)
 => (class :: <class>);
  %%redefine-complex-class
    (class, debug-name, module, abstract?, primary?, sealed?,
     superclasses, slots, #[], #[])
end function;

define function %redefine-complex-class
    (class :: <class>,
     debug-name, module, abstract?, primary?, sealed?,
     superclasses, slots, inherited-slots, keywords)
 => (class :: <class>);
  %%redefine-complex-class
    (class, debug-name, module, abstract?, primary?, sealed?,
     superclasses, slots, inherited-slots, keywords)
end function;


define method nonstructural-redefinition? (class :: <class>,
                                           superclasses :: <sequence>,
                                           slots :: <simple-object-vector>
                                             )
  superclasses = direct-superclasses(class)
    // @@@@@@ and they are the same metaclasses - done at compiletime?
    & (begin
         let oslots :: <simple-object-vector> = direct-slot-descriptors(class);
         let noslots :: <integer> = size(oslots);
         let nslots :: <integer> = size(slots);
         nslots == noslots
           & (every?(method (sd :: <slot-descriptor>)
                       let old = find-old-slot(sd, oslots);
                       old & (begin
                                let old :: <slot-descriptor> = old;
                                slot-allocation(old) == slot-allocation(sd)
                                  & same-specializer?(slot-type(old), slot-type(sd))
                                  & slot-setter(old) == slot-setter(sd)
                              end)
                     end method,
                     slots))
       end)
end method;


define method find-old-slot (sd :: <slot-descriptor>, slotvec :: <simple-object-vector>)
 => (ans :: false-or(<slot-descriptor>))
  block (gotcha)
    for (nsd :: <slot-descriptor> in slotvec) if (getter=(sd, nsd)) gotcha(nsd) end if end;
    #f
  end
end method;



define method %redefine-class-of-new-structure (class :: <class>, dependents :: <list>,
                                                #rest initargs)
 => (v :: false-or(<condition>))
  let u :: <subjunctive-class-universe> = make-empty-subjunctive-class-universe();
  let thisiclass :: <implementation-class>
    = apply(make, <implementation-class>,
            class: class,
            defer-cross-class-computations?: #t,
            subjunctive-class-universe: u,
            initargs);
  // This is the redefining-one-class optimization.  Just go straight through the
  // dependents making new implementation classes, although we do defer cross class
  // computations until we have our full SCU built up.
  for (c :: <class> in dependents)
    if (c ~== class)
      make(<implementation-class>,
           class: c,
           subjunctive-class-universe: u,
           defer-cross-class-computations?: #t,
           superclasses: direct-superclasses(c),
           slots: direct-slot-descriptors(c),
           inherited-slots: direct-inherited-slot-descriptors(c),
           keywords: direct-initialization-argument-descriptors(c))
    end if;
  end for;
  // Now, we've computed all new implementation classes;  do dependency-directed (by
  // direct superclasses) initialization of them.  This also does error checking.
  scu-initialize-all(do-implementation-class-cross-class-initializations, u);
  // Figure out the new known-joint info.  For classes with new implementation classes
  // we put it into the new implementation class, otherwise we remember an alist for
  // later updating.
  let joint-updates :: <table> = compute-known-joint(thisiclass, dependents, u);
  // Now we've got new implementation classes for all the classes that need to be redefined.
  // Most of the error checking has been performed, and we haven't yet trashed the existing
  // world.   Now nuke the existing classes, and install the new ones.
  // -- There is one screw area here, and that has to do with adding slot methods.  We do
  // -- not have a way to check for congruency of about-to-be-added slot methods with any
  // -- already defined generic functions in the "correct" environment (one in which the
  // -- type system reflects the redefinition and not the previous state).  So for now we'll
  // -- do slot method addition as we install the new class definitions.  Probably we should
  // -- install the new class definitions, then do all the slot method additions, and allow
  // -- individual bad ones to be skipped through judicious application of restarts.
  scu-do(invalidate-previous-implementation-class, u);
  scu-do(install-implementation-class, u);
  for (joint :: <list> keyed-by c in joint-updates)
    let c :: <class> = c;
    class-known-joint(c) := as(<simple-object-vector>, joint)
  end for;
  scu-do(add-slot-methods, u);
  #f
end method;

define function compute-known-joint (iclass :: <implementation-class>,
                                     dependents :: <list>,
                                     u :: <subjunctive-class-universe>)
 => (t :: <table>)
  let t :: <table> = make(<table>);
  let ndependents :: <list>
    = dependent-subclasses(tail(all-superclasses(iclass)),
                           dependent-subclasses
                             (tail(all-superclasses(class-implementation-class
                                                      (iclass-class(iclass)))),
                              dependents));
  for (c :: <class> in ndependents)
    for (subl :: <list> = tail(all-superclasses(c)) then tail(subl),
         until: empty?(subl) | head(subl) == <object>)
      let c1 :: <class> = head(subl);
      let ic1 :: <implementation-class> = scu-entry(c1, u);
      let j0 :: <list> = element(t, c1, default: #());
      local method loop (subl2 :: <list>, j1 :: <list>)
              if (empty?(subl2) | head(subl2) == <object>)
                unless (j0 == j1) element(t, c1) := j1 end
              else
                let c2 :: <class> = head(subl2);
                let ic2 :: <implementation-class> = scu-entry(c2, u);
                if (subiclass?(ic1, c1, ic2, c2) | member?(c2, j1))
                  loop(tail(subl2), j1)
                else
                  loop(tail(subl2), pair(c2, j1))
                end if
              end if
            end method;
      loop(tail(subl), j0)
    end for
  end for;
  t
end function;


define function invalidate-previous-implementation-class (ic :: <implementation-class>,
                                                          scu :: <subjunctive-class-universe>)
 => ()
  let old = class-implementation-class(iclass-class(ic));
  debug-assert(old ~== ic, "attempting to invalidate %= without a new one", ic);
  if (instance?(old, <implementation-class>))
    // @@@@ I'm not sure it's possible for this to not be true.  I don't remember
    // the reason for this check - should review whether there's any circumstance
    // at all where it might be necessary.  (Result of a failed make?)
    let old :: <implementation-class> = old;
    invalidate-implementation-class(old)
  end if
end function;


define variable *obsolete-instance-class* :: <class>
  = <miscellaneous-obsolete-instance>;


define method invalidate-implementation-class (ic :: <implementation-class>) => ()
  let newclass :: <class> = *obsolete-instance-class*;
  remove-implementation-class-slot-methods(ic);
  invalidate-class-instance?-iep(iclass-class(ic));
  iclass-class(ic) := newclass;
  let mic :: <implementation-class> = class-implementation-class(newclass);
  all-superclasses(ic) := all-superclasses(mic);
  class-known-joint(ic) := #[];
  direct-superclasses(ic) := direct-superclasses(mic);
  // class-subtype-bit(iclass-class(ic)) := class-subtype-bit(iclass-class(mic));
  mm-wrapper-subtype-mask(class-mm-wrapper(ic))
    := mm-wrapper-subtype-mask(class-mm-wrapper(mic));
  class-rcpl-vector(ic) := class-rcpl-vector(mic);
  class-rcpl-position(ic) := class-rcpl-position(mic);
  class-rcpl-other-positions(ic) := class-rcpl-other-positions(mic);
  direct-subclasses(ic) := #();
  iclass-dispatch-key(ic) := iclass-dispatch-key(mic);
end method;


define method remove-implementation-class-slot-methods (ic :: <implementation-class>) => ()
  local method find-and-remove-getter-method (g :: <generic-function>, sd :: <slot-descriptor>) => ()
          let spec :: <class> = slot-owner(sd);
          block (done)
            for (m :: <method> in generic-function-methods(g))
              if (spec == %method-specializer(m, 0))
                %remove-method(g, m);
                done()
              end if
            end for
          end
        end method;
  local method find-and-remove-setter-method (g :: <generic-function>, sd :: <slot-descriptor>) => ()
          let spec0 :: <type> = slot-type(sd);
          let spec1 :: <class> = slot-owner(sd);
          block (done)
            for (m :: <method> in generic-function-methods(g))
              if (same-specializer?(spec0, %method-specializer(m, 0))
                    & spec1 == %method-specializer(m, 1))
                %remove-method(g, m);
                done()
              end if
            end for
          end
        end method;
  for (sd :: <slot-descriptor> in direct-slot-descriptors(ic))
    // @@@@@ ????? What to do with virtual slots?
    if (~instance?(sd, <virtual-slot-descriptor>))
      let g = slot-getter(sd);
      let s = slot-setter(sd);
      if (g)
        let g :: <generic-function> = g;
        find-and-remove-getter-method(g, sd);
      end if;
      if (s)
        let s :: <generic-function> = s;
        find-and-remove-setter-method(s, sd);
      end if;
    end if;
  end for;
end method;



define method overwrite-slot-descriptor (osd :: <slot-descriptor>, nsd :: <slot-descriptor>)
 => ()
  (with-lock ($slot-initial-data-lock)
     begin
       init-data-slot(osd) := init-data-slot(nsd);
       init-keyword(osd) := init-keyword(nsd);

       // These should be properties slots copied as a block.
       init-supplied?(osd) := init-supplied?(nsd);
       init-evaluated?(osd) := init-evaluated?(nsd);
       init-value?(osd) := init-value?(nsd);
       init-keyword-required?(osd) := init-keyword-required?(nsd);
     end
  end)
end method;



define method %redefine-class-attributes-only (class :: <class>, dependents :: <list>,
                                               #key slots :: <simple-object-vector> = #[],
                                               inherited-slots :: <simple-object-vector> = #[],
                                               keywords :: <simple-object-vector> = #[],
                                               superclasses :: <simple-object-vector> = #[])
 => (v :: false-or(<condition>))
  for (osd :: <slot-descriptor> in direct-slot-descriptors(class))
    let nsd :: <slot-descriptor>
      = find-old-slot(osd, slots) | error("Bug! Class %= is a structural redefinition?", class);
    overwrite-slot-descriptor(osd, nsd)
  end for;
  for (dep :: <class> in dependents)
    compute-defaulted-initialization-arguments(class-implementation-class(dep),
                                               map-as(<list>, method(x) class-implementation-class(x) end,
                                                      all-superclasses(dep)),
                                               $empty-subjunctive-class-universe)
  end for;
  #f
end method;

