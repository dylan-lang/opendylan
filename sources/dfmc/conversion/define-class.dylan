Module:   dfmc-conversion
Synopsis: The class definition processor.
Author:   Paul Haahr, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Metaclass framework.

// Class definitions can specify a metaclass option. The value of that
// option is currently a non-module scoped name that can is resolved
// to a compile-stage metaclass in the compiler. 

// This stuff is only used by the FFI at the moment.

define constant $compiler-metaclass-map = make(<table>);

define method define-compiler-metaclass (tag :: <symbol>, metaclass) => ()
  element($compiler-metaclass-map, tag) := metaclass
end method;

define method lookup-compiler-metaclass (tag :: <symbol>) => (metaclass)
  element($compiler-metaclass-map, tag, default: #f)
    | error("Failed to resolve the compiler metaclass %=.", tag);
end method;

define-compiler-metaclass(#"<value-class>", <&value-class>);
define-compiler-metaclass(#"<function-class>", <&function-class>);
define-compiler-metaclass(#"<virtual-class>", <&virtual-class>);


//// Class modeling.

define serious-program-warning <non-class-superclass-expression>
  slot condition-form,
    init-keyword: form:;
  slot condition-superclass-expression,
    init-keyword: superclass-expression:;
  slot condition-superclass-value,
    init-keyword: superclass-value:;
  format-string
    "The superclass %= of %= evaluates to the non-class object: %=.";
  format-arguments
    superclass-expression, form, superclass-value;
end serious-program-warning;

define serious-program-warning <nonlocal-sealed-superclass>
  slot condition-form,
    init-keyword: form:;
  slot condition-superclass-expression,
    init-keyword: superclass-expression:;
  slot condition-superclass-value,
    init-keyword: superclass-value:;
  format-string
    "The superclass %= of %= is defined in another library, and sealed.";
  format-arguments
    superclass-expression, form, superclass-value;
end serious-program-warning;

define program-warning <inaccessible-open-definition>
  slot condition-binding, required-init-keyword: binding:;
  format-string "Definition of %s is declared open, but it is not exported from this library\n"
                "(This warning can be avoided by declaring the definition \"dynamic\" instead of \"open\")";
  format-arguments binding;
end program-warning;


define function force-sealing? (form)
  when (form.form-sealed-if-private?)
    let binding = form-variable-binding(form);
    unless (binding-accessible-to-other-libraries?(binding))
      // debug-out(#"seal", "Sealing %s\n", binding);
      #t
    end;
  end;
end;

define function form-sealable? (form :: <variable-defining-form>)
  form-declared-sealed?(form) | force-sealing?(form)
end function;


define compiler-sideways method compute-and-install-form-model-objects
    (form :: <class-definition>) => ()
  form-evaluation-tried-and-failed?(form) := #f;
  unless (form-sealed-if-private?(form) |
	  member?(#"dynamic", form-adjectives(form)) |
	  binding-accessible-to-other-libraries?(form-variable-binding(form)))
    note(<inaccessible-open-definition>,
	 binding: form-variable-binding(form),
	 source-location: form-source-location(form));
  end;
  if (form-dynamic?(form))
    compute-and-install-form-dynamic-init-method(form);
  else
    compute-and-install-form-model-objects-statically(form);
  end;
end method;

define method compute-and-install-form-model-objects-statically
    (form :: <class-definition>) => ()
  let variable-name = form-variable-name(form);
  let mylib = form-library(form);
  let superclass-models
    = ^top-level-eval-sequence
        (form-superclass-expressions(form), on-failure: #f);
  if (~superclass-models 
	| ~every?(method (x) 
		    instance?(x, <&class>)
		      & ~ ^class-incremental?(x)
		      & (mylib == model-library(x) | ~^class-sealed?(x))
		  end,
		  superclass-models))
    if (superclass-models)
      // debug-out(#"gsb", ">>> -- Computed superclass-models for %= as %=\n",
      //          form, superclass-models);
      let mylib = form-library(form);
      for (model in superclass-models, 
	   expr in form-superclass-expressions(form))
	if (~instance?(model, <&class>))
	  note(<non-class-superclass-expression>,
	       source-location: fragment-source-location(expr),
	       form: form-variable-name(form),
	       superclass-expression: expr,
	       superclass-value: model);
	elseif (mylib ~== model-library(model) & ^class-sealed?(model))
	  note(<nonlocal-sealed-superclass>,
	       source-location: fragment-source-location(expr),
	       form: form-variable-name(form),
	       superclass-expression: expr,
	       superclass-value: model)
	end if
      end for
    else
      debug-out(#"gsb", ">>> -- Couldn't get superclass-models for %=\n", form);
    end if;
    debug-out(#"dynamic",
	      ">>> Retreating to the dynamic case for %=\n", form);
    form-evaluation-tried-and-failed?(form) := #t;
    compute-and-install-form-dynamic-init-method(form);
  else
    let superclass-models = as(<simple-object-vector>, superclass-models);
    let primary? = form-primary?(form);
    let sealed? = form-sealable?(form);
    let (compiler-metaclass, metaclass-initargs)
      = eval-metaclass-spec(form-metaclass-spec(form));
    // Slot specs are allowed to forward reference the class, so we
    // make and install the class model now and fill in the slots later.
    let model 
      = apply(^make, compiler-metaclass,
              definition:                          form,
              debug-name:                          
                mapped-model(as-lowercase(as(<string>, variable-name))),
              superclasses:                          
                mapped-model(superclass-models),
              slots:                               #(),
              abstract?:                           form-abstract?(form),
              sealed?:                             sealed?,
              primary?:                            primary?,
	      incremental?:                        #f,
              complete?:                           #f,
	      type-complete?:                      #t,
              value-class?:                        
                compiler-metaclass == <&value-class>,
              slots-have-fixed-offsets?:           primary?,
              slots-have-fixed-offsets?-computed?: primary?,
              metaclass-initargs);
    ^setup-cpl-rcpl(model, ^compute-class-precedence-list(model)) ;
    ^check-inheritance(model);
    define-model-object(variable-name, model);
    /*
    // We may yet bail if the slots aren't evaluable "enough".
    if (~all-clauses-evaluable?(form))
      debug-out(#"dynamic",
		">>> Retreating to the dynamic case on slots for %=\n", form);
      form-evaluation-tried-and-failed?(form) := #t;
      retract-model-object(variable-name);
      compute-and-install-form-dynamic-init-method(form);
    end;    
    */
    let mylib = form-library(form);
    if (any?(method (x) mylib ~== model-library(x) end, superclass-models))
      let code 
	= (with-expansion-source-form(form)
	     let definer = dylan-value(#"%add-class");
	     #{ ?definer(?model) }
	   end with-expansion-source-form);
      let init-model = convert-top-level-initializer(code);
      form-system-init-method(form) := init-model;
    end if;
  end;
end method;

define constant $min-rcpl-size = 6;


define function ^setup-cpl-rcpl (new-cls :: <&class>, cpl) => ();
  new-cls.^all-superclasses := mapped-model (cpl) ;
  let  self-pos = cpl.size - 1;
  let  rcpl = make(<simple-object-vector>, 
		   size: max(self-pos + 1, $min-rcpl-size),
		   fill: #f);
  let  mylib  = new-cls.model-library;
  new-cls.^class-rcpl-position := self-pos; // This is always the smallest position.
  for (pos from self-pos to 0 by -1, super in cpl)
    rcpl [pos] := super ;
    if (pos > 0 & pos < self-pos)  // uninterested if self or <object>
      if (super.model-library == mylib)
	^augment-rcpl-position-data(super, pos)
      end
    end
  end;
  new-cls.^class-rcpl-vector := mapped-model (rcpl);
end;

define function add-position-sorted (pos :: <integer>, oposns :: <list>) => (posns :: <list>)
  local method phu (prev :: false-or(<pair>))
	  if (prev == #f)
	    pair(pos, oposns)
	  else
	    let prev :: <pair> = prev;
	    tail(prev) := pair(pos, tail(prev));
	    oposns
	  end if
	end method;
  local method loop (posns :: <list>, prev :: false-or(<pair>))
	  if (posns.empty?)
	    phu(prev)
	  else
	    let elem :: <integer> = posns.head;
	    if (pos == elem)
	      oposns
	    elseif (pos > elem)
	      loop(tail(posns), posns)
	    else
	      phu(prev)
	    end if
	  end if
	end method;
  loop(oposns, #f);
end function;

  
define function ^augment-rcpl-position-data (cls :: <&class>, pos :: <integer>) => ()
  if (pos ~== cls.^class-rcpl-position)
    let posns = cls.^class-incremental-rcpl-positions;
    let new-posns :: <list> = add-position-sorted(pos, posns);
    if (posns ~== new-posns) cls.^class-incremental-rcpl-positions := new-posns end;
  end if
end;

define function ^ensure-class-complete (class :: <&class>) => ()
  unless (^class-complete?(class))
    for (super in ^direct-superclasses(class))
      ^ensure-class-complete(super)
    end;
    ^ensure-slots-initialized(class);
    optimize-slot-initializers(class);
    // do this after optimizing slot initializers
    ^compute-defaulted-initialization-arguments(class);
    ^class-complete?(class) := #t;
    let iclass = ^class-implementation-class(class);
    let inst? = (~^class-abstract?(class) & member?(dylan-value(#"<object>"), ^all-superclasses(class)));
    ^iclass-instantiable?(iclass) := inst?;
    debug-out(#"gsb", ">>> %= %s, props=%x.\n", class, 
              if (inst?) "IS instantiable" else "is NOT instantiable" end,
              ^class-properties(iclass));
  end;
end function;

define compiler-sideways method finish-installing-form-model-objects
    (form :: <class-definition>) => ()
  unless (form-dynamic?(form))
    let class = form-model-object(form);
    ^ensure-class-complete(class);
  end;
end;

//define method finish-model (class :: <&class>) => ()
//  // format-out("Finish-model called on %=\n", class);
//  let form = class.model-definition;
//  let mylib = class.model-library;
//  if (form-system-init-method(form))
//    // format-out("    ==> Oops, it's been done already!\n")
//  else
//    class.^class-rcpl-other-positions 
//      := mapped-model(as(<simple-object-vector>, class.^class-incremental-rcpl-positions));
//    let sups :: <list> = ^all-superclasses(class);
//    local method find-fixups (supers :: <list>, pos :: <integer>, fixups :: <list>)
//	    if (pos <= 0)
//	      if (fixups ~== #())
//		// format-out("  ==> %= needs rcpl position fixups %=\n", class, fixups);
//		let vec = mapped-model(as(<simple-object-vector>, fixups));
//                form-system-init-method(form) 
//                  := convert-top-level-initializer
//                       (#{ ?=augment-rcpl-position-data-kludgey(?vec) });
//	      end if
//	    else
//	      let sup :: <&class> = head(supers);
//	      find-fixups(tail(supers), pos - 1,
//			  if (sup.model-library ~== mylib
//				& ~member?(pos, ^class-incremental-rcpl-positions(sup)))
//			    pair(mapped-model(sup), pair(pos, fixups))
//			  else
//			    fixups
//			  end if)
//	    end if
//	  end method;
//    find-fixups(tail(sups), size(sups) - 2, #());
//    ^instance?-iep(class); // Force computation reference of this.
//  end if
//end method;

define class <heap-deferred-all-classes-model> (<heap-deferred-model>) end;

install-&class-mapping(<heap-deferred-all-classes-model>, #"<simple-object-vector>");

// Finish-model for classes now also has a role to play in interactive
// mode, adding the explicit redefinition of the variable in the case
// where the variable had a previous (non-class) definition.
define function finish-class-models (ld :: <library-description>, form-mapper :: <function>)
 => (code)
  let rcpl-table :: <table> = make(<table>);
  let joint-table :: <table> = make(<table>);
  let root-lib? = dylan-library-library-description?(ld);
  local method fubar (c :: <&class>)
	  if (~instance?(c, <&virtual-class>))
	    // Force direct subclasses to be computed. This is important,
            // the heaper/linker has problems without it.
            ^direct-subclasses(c);
	    if (^all-subclasses-if-sealed(c))
	      let ic :: <&implementation-class> = ^class-implementation-class(c);
	      ^iclass-subclasses-fixed?(ic) := #t;
	    end if;
	    let sups :: <list> = ^all-superclasses(c);
	    let others = c.^class-incremental-rcpl-positions;
	    if (others ~== #())
	      // format-out("  ---- Class %= has other-positions %=\n", c, others);
	      c.^class-rcpl-other-positions := mapped-model(as(<simple-object-vector>, others));
	    end if;
	    unless (root-lib?)
	      // Look over the superclasses and make note of those that are going to need
	      // to have their rcpl positions vector augmented at load time.
	      for (sup :: <&class> in tail(sups),
		   pos :: <integer> = size(sups) - 2 then pos - 1,
		   while: pos > 0)
		if (sup.model-library ~== ld
		      & pos ~== ^class-rcpl-position(sup)
		      & ~member?(pos, ^class-incremental-rcpl-positions(sup)))
		  let known :: <list> = element(rcpl-table, sup, default: #());
		  let new-known :: <list> = add-position-sorted(pos, known);
		  if (known ~== new-known)
		    element(rcpl-table, sup) := new-known
		  end if
		end if;
	      end for;
	    end;
	    // Compute modifications to the class-known-joint sets of this class's superclasses.
	    for (subl :: <list> = tail(sups) then tail(subl), 
		 until: empty?(subl))
	      let c1 :: <&class> = head(subl);
	      let c1local? = c1.model-library == ld;
	      let c1othersups = tail(c1.^all-superclasses);
	      for (c2 :: <&class> in tail(subl))
		if (~member?(c2, c1othersups))
		  // Only memoize if c2 isn't a superclass of c1.
		  let j1 :: <list> = element(joint-table, c1, default: #());
		  if (~member?(c2, j1))
		    let j2 :: <list> = element(joint-table, c2, default: #());
		    if (~member?(c1, j2))
		      // c1 and c2 are not already known to occur together in anyone's CPL.
		      // Add one to the class-known-joint set of the other, biasing the choice
		      // towards the one defined in this library so the modification can be made
		      // to the model rather than at load time.
		      if (c1local? | c2.model-library ~== ld)
			element(joint-table, c1) := pair(c2, j1)
		      else
			element(joint-table, c2) := pair(c1, j2)
		      end if
		    end if
		  end if
		end if
	      end for
	    end for
	  end if
	end method;
  form-mapper(ld, fubar);
//  form-mapper(ld, ^instance?-iep);// Force computation reference of this.
  let classvec = make(<heap-deferred-all-classes-model>);
  let code = if (root-lib?)
	       #{ *implementation-classes-by-key* := ?classvec ; 
		  *next-unique-dispatch-key* := size(*implementation-classes-by-key*) ; }
	     else
	       #{ initialize-class-dispatch-keys-vectored ( ?classvec ) ; }
	     end if;
  local method add-code (fn, class-vec :: <stretchy-vector>, data-vec :: <stretchy-vector>)
	  unless (empty?(class-vec))
	    if (class-vec.size == 1)
	      let cl = class-vec[0];
	      let data = mapped-model(data-vec[0]);
	      code := #{ ?code ?fn(?cl, ?data) ; };
	    else
	      let classv = mapped-model(as(<simple-object-vector>, class-vec));
	      let datav = mapped-model(as(<simple-object-vector>, data-vec));
	      code := #{ ?code do(?fn, ?classv, ?datav) ; };
	    end if;
	  end unless;
	end method;
  unless (empty?(rcpl-table))
    let p-vec = make(<stretchy-vector>);
    let p-class-vec = make(<stretchy-vector>);
    for (stuff :: <list> keyed-by c in rcpl-table)
      // format-out("  ---- Class %= needs rcpl-position fixups %=\n", c, stuff);
      add!(p-class-vec, c);
      add!(p-vec, as(<simple-object-vector>, stuff));
    end;
    add-code(dylan-value(#"augment-rcpl-position-data-multiple"), p-class-vec, p-vec);
  end unless;
  let j-vec = make(<stretchy-vector>);
  let j-class-vec = make(<stretchy-vector>);
  for (joint :: <list> keyed-by c /* :: <&class> */ in joint-table)
    if (c.model-library == ld)
      c.^class-known-joint := mapped-model(as(<simple-object-vector>, joint));
    else
      add!(j-class-vec, c);
      add!(j-vec, as(<simple-object-vector>, joint));
    end;
  end for;
  add-code(dylan-value(#"augment-class-known-joint"), j-class-vec, j-vec);
  code
end function;

define compiler-sideways method retract-form-model-objects (form :: <class-definition>) => ()
  library-description-system-class-init-code(form-library(form)) := #f;
  next-method()
end method;


define method eval-metaclass-spec 
    (spec == #f) => (compiler-metaclass, metaclass-initargs)
  values(<&class>, #())
end method;

define method eval-metaclass-spec 
    (spec) => (compiler-metaclass, metaclass-initargs)
 let metaclass 
   = lookup-compiler-metaclass(spec-metaclass-name(spec));
 values(metaclass, spec-metaclass-initargs(spec))
end method;

/* TODO: OBSOLETE?
define method eval-property-list (property-list)
  collecting ()
    for (cursor = property-list then cursor.tail.tail, 
         until: empty?(cursor))
      let key = cursor.first;
      let val-expression = cursor.second;
      collect(key);
      collect(^top-level-eval(val-expression));
    end;
  end;
end method;
*/

//// Initializer methods

define method install-method-signature 
    (m :: <&initializer-method>, 
       form :: <method-defining-form>, sig :: <&signature>)
 => ()
  next-method();
  // We have to do this now, before anyone gets to look at the signature,
  // because the signature is fake until the set of init keywords is 
  // computed during body generation.
  maybe-compute-and-install-method-dfm(m);
  // The first specializer is the class in question.
  // let class = sig.^signature-values.first;
  // ^class-constructor(class) := m;
end method;

define method compute-method-body 
    (m :: <&initializer-method>) => (body-fragment)
  // The second specializer is the class in question.
  let class = m.^function-signature.^signature-values.first;
  // Only attempt this for simple classes with only instance slots right now.
  ^ensure-class-complete(class);
  if (^ensure-slots-initialized(class) 
        & empty?(^class-slot-descriptors(class)))
    // break("Compute the initializer body for: %=", class);
    collecting (key-specs, set-specs)
      for (slotd in ^instance-slot-descriptors(class))
        let (key-spec, set-spec) 
          = compute-slot-initialization-code(class, slotd);
        if (key-spec) collect-into(key-specs, key-spec); end;
        collect-into(set-specs, set-spec);
      end;
      let key-specs = collected(key-specs);
      let set-specs = collected(set-specs);
      // format-out("Keys: %=\n", key-specs);
      // format-out("Sets: %=\n", set-specs);
      let default-init-args = ^defaulted-initialization-arguments-slot(class);
      let no-defaults? 
        = instance?(default-init-args, <simple-object-vector>)
            & empty?(default-init-args);
      let new-signature-fragment
        = if (no-defaults?)
            #{ (class :: <class>, #rest init-args,
                  #key ??key-specs, ..., #all-keys)
               => (object :: ?class) };
          else
            #{ (class :: <class>, #rest init-args, #key, #all-keys) 
                 => (object :: ?class) };
          end;
      let (new-signature, empty-body)
        = parse-method-signature
            (#{ default-initialize }, new-signature-fragment);
      let signature-model
        = compute-signature(m.model-definition, new-signature);
      signature-spec(m) := new-signature;
      ^function-signature(m) := signature-model;
      ^class-constructor(class) := m;
      let allocation
        = if (^repeated-slot-descriptor(class))
            #{ allocate-instance(class, init-args) }
          else
            // #{ system-allocate-simple-instance(?class) }
            let instance-size = ^instance-storage-size(class);
            let raw-total-size
              = make-raw-literal
                  (dylan-value(#"$number-header-words") + instance-size);
            let wrapper           = ^class-mm-wrapper(class);
            let raw-instance-size = make-raw-literal(instance-size);
            let raw-zero          = make-raw-literal(0);
            #{ primitive-object-allocate-filled
                 (?raw-total-size, ?wrapper, ?raw-instance-size,
                    %unbound, ?raw-zero, ?raw-zero, %unbound) }
          end;
      if (no-defaults?)
        #{ begin
             let class = ?class;
             let object :: ?class = ?allocation;
             begin ??set-specs; ... end;
             apply(initialize, object, init-args);
             object
           end }
      else
        #{ begin 
             local method defaulted-initialize 
                 (object :: ?class, 
                    #rest init-args, #key ??key-specs, ..., #all-keys)
               begin ??set-specs; ... end;
               apply(initialize, object, init-args);
             end method;
             let class = ?class;
             let init-args 
               = concatenate-2
                   (init-args, class.defaulted-initialization-arguments);
             let object :: ?class = ?allocation;
             apply(defaulted-initialize, object, init-args);
             object
           end }
      end;
    end;
  else
    ^class-constructor(class) := dylan-value(#"default-class-constructor");
    #{ error("Punt complex class constructor for %= called.", ?class) }
  end
end method;

// TODO: CORRECTNESS: Type check assertions.

define method compute-slot-initialization-code 
    (class :: <&class>, slotd :: <&slot-descriptor>) => (key-spec, set-spec)
  let name 
    = make-unique-local-variable-name-fragment
        (model-variable-name(^slot-getter(slotd)));
  let keyword = ^init-keyword(slotd);
  let initd = ^effective-initial-value-descriptor(slotd, class);
  let key-spec 
    = keyword 
        & if (^init-supplied?(initd))
            let init 
              = compute-slot-initialization-code-for-default-value
                  (class, slotd);
            #{ ?keyword ?name = ?init }
          elseif (^init-keyword-required?(slotd))
            #{ ?keyword ?name = error("Missing init keyword %=", ?keyword) }
          else
            #{ ?keyword ?name = ?$unbound }
          end;
  let offset = ^slot-offset(slotd, class);
  let set-spec
    = if (key-spec)
        let type = ^slot-type(slotd);
        let type-check
          = if (type == dylan-value(#"<object>"))
              #{ }
            else
              // This optimization also works around a gotcha - by an abuse,
              // repeated slot size slot descriptors have the repeated slot
              // as their definition, so the type expression is the contents
              // type not the size type (always integer).
              let (inlineable?, type) = inlineable?(type);
              let type-expression 
                = if (inlineable?)
                    type
                  else
                    spec-type-expression(model-definition(slotd))
                  end;
              // It gets ugly if we might get unbound since we have to not
              // do the type check in that case. In practice people don't
              // seem to leave their slots with the potential of being
              // unbound yet keyword initializable that often, but we'll 
              // have to see...
              if (^init-keyword-required?(slotd) | ^init-supplied?(initd))
                #{ let ?name :: ?type-expression = ?name; }
              else
                #{ let ?name 
                     = if (?name == ?$unbound) 
                         ?name
                       else
                         let ?name :: ?type-expression = ?name;
                         ?name
                       end; }
              end;
            end;
        #{ ?type-check %slot-value-setter(?name, object, ?slotd, ?offset) }
      else
        if (^init-supplied?(initd))
          let init
            = compute-slot-initialization-code-for-default-value
                (class, slotd);
          #{ %slot-value-setter(?init, object, ?slotd, ?offset) }
        elseif (^slot-type(slotd) == dylan-value(#"<raw-machine-word>"))
          let raw-zero = make-raw-literal(0);
          #{ %slot-value-setter(?raw-zero, object, ?slotd, ?offset) }
        else
          // Is this necessary, or does allocation fill in unbound?
          #{ %slot-value-setter(?$unbound, object, ?slotd, ?offset) }
        end
      end;
  values(key-spec, set-spec)
end method;

define function copy-default-value-method (data) => (data)
  // This should be ensured by the way the slot init data is
  // constructed in the first place. If it's not a function,
  // it wouldn't have got folded down to evaluated status.
  debug-assert
    (instance?(data, <&lambda>), 
       "Anonymous slot init function must be a lambda.");
  ensure-method-dfm(data);
  let copier = current-dfm-copier(estimated-copier-table-size(data));
  let data   = deep-copy(copier, data);
  // PERMIT IT TO BE INLINED -- EFFECTIVELY NOT TOP LEVEL ANYMORE
  lambda-top-level?(data) := #f;
  data
end function;

define method compute-slot-initialization-code-for-default-value
    (class :: <&class>, slotd :: <&slot-descriptor>) => (default-code)
  let initd = ^effective-initial-value-descriptor(slotd, class);
  let data = ^init-data-slot(initd);
  let init 
    = if (^init-evaluated?(initd))
        if (^init-value?(initd))
          data
	else
          let (ok?, local-data) = inlineable?(data);
	  if (ok?)
	    #{ (?local-data()) }
	  else
	    let data = copy-default-value-method(data);
	    #{ (?data()) }
	  end
        end
      else
        if (^init-value?(initd))
          // Eval once.
          #{ make-method-init-value(?initd) }
        else
	  let data = copy-default-value-method(data);
	  #{ ((?data())()) }
        end
      end;
  init
end method;

//// Incremental mode expansion

define method form-in-place-redefinition?
    (form :: <variable-defining-form>) => (well? :: <boolean>)
  #f
end method;

define method compute-and-install-form-dynamic-init-method
    (form :: <top-level-form>) => ()
  if (form-binding-guaranteed-initialized?(form))
    if (~form-in-place-redefinition?(form))
      let name = form-variable-name(form);
      let object = compute-form-hollow-object(form);
      define-hollow-object(name, object);
      if (form-redefinition?(form))
        form-system-init-method(form)
          := convert-top-level-initializer
               (#{ %initialize-binding-type(?name, <object>);
                   %initialize-binding(?name, ?object) });
      end;
    end;
  end;
  let code 
    = // with-expansion-module (fragment-module(form-variable-name(form)))
        compute-form-dynamic-init-code(form);
      // end;
  let init-model = convert-top-level-initializer(code);
  form-init-method(form) := init-model;
end method;

define method compute-form-dynamic-init-code
    (form :: <class-definition>) => (computed-method)
  let name
    = form-variable-name(form);
  let supers
    = form-superclass-expressions(form);
  let slots 
    = compute-specs-dynamic-init-args-vector(form-slot-specs(form));
  let inherited-slots 
    = compute-specs-dynamic-init-args-vector(form-inherited-slot-specs(form));
  let keywords 
    = compute-specs-dynamic-init-args-vector(form-keyword-specs(form));
  let complex?
    = ~empty?(inherited-slots) | ~empty?(keywords);
  let constructor 
    = if (form-in-place-redefinition?(form))
	let abstract
	  = form-abstract?(form);
	let primary
	  = form-primary?(form);
	let sealed
	  = form-sealable?(form);
	let module 
	  = form-module-model(form);
	if (complex?)
	  let definer = dylan-value(#"%redefine-complex-class");
	  #{ ?definer
	       (?name,
		?"name",
		?module,
		?abstract,
		?primary,
		?sealed,
		immutable-vector(??supers, ...),
		immutable-vector(??slots, ...),
		immutable-vector(??inherited-slots, ...),
		immutable-vector(??keywords, ...)) };
	else 
	  let definer = dylan-value(#"%redefine-class");
	  #{ ?definer
	       (?name,
		?"name",
		?module,
		?abstract,
		?primary,
		?sealed,
		immutable-vector(??supers, ...),
		immutable-vector(??slots, ...)) };
	end if
      else 
	if (complex?)
	  let definer = dylan-value(#"%define-complex-class");
	  #{ ?definer
	       (?name,
		immutable-vector(??supers, ...),
		immutable-vector(??slots, ...),
		immutable-vector(??inherited-slots, ...),
		immutable-vector(??keywords, ...)) };
	else 
	  let definer = dylan-value(#"%define-class");
	  #{ ?definer
	       (?name,
		immutable-vector(??supers, ...),
		immutable-vector(??slots, ...)) };
	end if
      end if;
  // #{ %initialize-binding(?name, ?constructor); }
  constructor
end method;

define method compute-and-install-form-dynamic-slot-init-method
    (form :: <class-definition>) => ()
  let code = compute-form-dynamic-slot-init-code(form);
  let init-model = convert-top-level-initializer(code);
  form-init-method(form) := init-model;
end method;

define method compute-form-dynamic-slot-init-code
    (form :: <class-definition>) => (computed-method)
  // Just do the whole thing for now.
  compute-form-dynamic-init-code(form);
end method;

define function compute-specs-dynamic-init-args-vector
    (specs :: <sequence>) => (fragment)
  collecting ()
    for (spec in specs)
      let init-args = compute-spec-dynamic-init-args(spec);
      collect(#{ immutable-vector(?init-args) });
    end;
  end;
end function;

define method compute-spec-dynamic-init-args
    (spec :: <slot-initial-value-spec>) => (args)
  if (~spec-init-supplied?(spec))
    #{ }
  else
    let expression = spec-init-expression(spec);
    if (spec-init-expression?(spec))
      #{ init-function: method () ?expression end }
    elseif (spec-init-value?(spec))
      #{ init-value: ?expression }
    else
      #{ init-function: ?expression }
    end;
  end;
end method;

define method compute-spec-dynamic-init-args
    (spec :: <slot-keyword-initialization-spec>) => (args)
  let initial-value-args = next-method();
  let keyword = spec-init-keyword(spec);
  if (~keyword)
    initial-value-args
  elseif (spec-init-keyword-required?(spec))
    #{ required-init-keyword: ?keyword, ?initial-value-args }
  else
    #{ init-keyword: ?keyword, ?initial-value-args }
  end;
end method;

define method compute-spec-dynamic-init-args
    (spec :: <slot-definition>) => (args :: <template>)
  let init-stuff = next-method();
  let getter = spec-getter(spec);
  let setter = spec-setter(spec);
  let type = spec-type-expression(spec);
  let allocation = spec-allocation(spec) | instance:;
  let sealed = spec-sealed?(spec);
  let atomic = spec-atomic?(spec);
  #{ getter:        ?getter,
     setter:        ?setter,
     // deferred-type: method () ?type end,
     type:          ?type,
     allocation:    ?allocation,
     sealed?:       ?sealed,
     atomic?:       ?atomic,
     ?init-stuff }
end method;

define method compute-spec-dynamic-init-args
    (spec :: <repeated-slot-definition>) => (args :: <template>)
  let slot-stuff = next-method();
  let size-getter = spec-size-getter(spec);
  let keyword = spec-size-init-keyword(spec);
  let keyword-stuff
    = if (keyword) #{ size-init-keyword: ?keyword } else #{ } end;
  let size-init-stuff 
    = if (spec-size-init-supplied?(spec))
        #{ size-init-function: method () ?spec end, ?keyword-stuff }
      else
        keyword-stuff
      end;
  #{ size-getter: ?size-getter,
     ?size-init-stuff,
     ?slot-stuff }
end method;

define method compute-spec-dynamic-init-args
    (spec :: <inherited-slot-spec>) => (args :: <template>)
  let init-stuff = next-method();
  let getter = spec-getter(spec);
  #{ getter: ?getter,
     ?init-stuff }
end method;

define method compute-spec-dynamic-init-args
    (spec :: <init-arg-spec>) => (args :: <template>)
  let init-stuff = next-method();
  let type = spec-type-expression(spec);
  #{ // deferred-type: ?getter,
     type: ?type,
     ?init-stuff }
end method;

// Hollow class objects.

define compiler-sideways method form-binding-guaranteed-initialized?
   (form :: <class-definition>) => (well? :: <boolean>)
  #t
end method;

define compiler-sideways method compute-form-hollow-object
   (form :: <class-definition>) => (model :: <&class>)
  let variable-name = form-variable-name(form);
  let primary? = form-primary?(form);
  let (compiler-metaclass, metaclass-initargs)
    = eval-metaclass-spec(form-metaclass-spec(form));
  let model 
    = apply(^make, compiler-metaclass,
            definition:                          form,
            debug-name:                          
              mapped-model(as-lowercase(as(<string>, variable-name))),
	    module:                              form-module-model(form),
            superclasses:                        #[],
            slots:                               #(),
            abstract?:                           form-abstract?(form),
            sealed?:                             form-sealable?(form),
            primary?:                            primary?,
	    complete?:                           #f,
	    type-complete?:                      #f,
            incremental?:                        #t,
	    slots-have-fixed-offsets?:           primary?,
	    slots-have-fixed-offsets?-computed?: primary?,
            metaclass-initargs);
  ^class-constructor(model) := dylan-value(#"default-class-constructor");
  model
end method;

define method form-in-place-redefinition?
    (form :: <class-definition>) => (well? :: <boolean>)
  let binding = form-variable-binding(form);
  binding-previously-defined?(binding)
    & instance?(binding-previous-definition(binding), form.object-class);
end method;
