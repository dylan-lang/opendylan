Module:   dfmc-conversion
Synopsis: The compile-time class protocol.
Author:   Keith Playford, from Jonathan's run-time code.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Class initialization.

define compiler-sideways method ^initialize-class
    (class :: <&class>, #rest args, #key superclasses = #(), slots = #(), #all-keys)
  // need to update the following use private-^class-implementation-class
  // debug-assert(~slot-initialized?(class, ^class-implementation-class), "wtf?")
  ^class-module(class) := model-module-model(class);
  ^class-implementation-class(class) := apply(^make, <&implementation-class>,
					      class: class,
					      definition: #f,
					      args);
  let sups :: <simple-object-vector> = as(<simple-object-vector>, superclasses);
  class.^direct-superclasses := mapped-model(sups);
  if (dylan-library-library-description?(model-library(class)))
    let pos = position($dylan-system-subtype-bit-type-names,
                       as(<symbol>, ^debug-name(class)));
    if (pos) ^class-subtype-bit(class) := ash(1, pos) end;
  end if;
end method ^initialize-class;

define sideways method ^instance?-function (c :: <&class>)
  if (c == dylan-value(#"<object>"))
    #"<object>-class-instance?"
  elseif (c == dylan-value(#"<byte-character>"))
    #"<byte-character>-instance?"
  elseif (c == dylan-value(#"<unicode-character>"))
    #"<unicode-character>-instance?"
  elseif (c == dylan-value(#"<integer>"))
    #"<integer>-instance?"
  elseif (^sealed-with-no-subclasses?(c))
    // This is only valid for indirect objects, too;  it's essential that any
    // direct objects be handled otherwise.
    #"class-instance?-initial"
  elseif (^class-subtype-bit(c) ~== 0)
    #"masked-class-instance?"
  else
    let poslist :: <list> = ^class-incremental-rcpl-positions(c);
    if (poslist == #())
      let pos :: <integer> = ^class-rcpl-position(c);
      if (pos < $min-rcpl-size)
	#"class-instance?-rcpl-single-small"
      else
	#"class-instance?-rcpl-single-large"
      end if
    else
      #"general-rcpl-class-instance?"
    end if
  end if
end method;

define function ^ensure-slots-initialized-dynamically
    (class :: <&class>) => ()
  debug-out(#"dynamic",
	    ">>> Retreating to dynamic case slots for  %=\n", class);
  let form = model-definition(class);
  with-dependent-context ($compilation of form)
    class.^slots-initialized-state := #"tried-and-failed";
    ^finalize-slots(class);
    form-evaluation-tried-and-failed?(form) := #t;
    compute-and-install-form-dynamic-slot-init-method(form);
  end;
end function;

define function ^ensure-slots-initialized 
    (class :: <&class>) => (well? :: <boolean>)
  select (class.^slots-initialized-state)
    #"tried-and-failed"
      => #f;
    #"done"
      => #t;
    #"uninitialized"
      => if (~every?
               (^ensure-slots-initialized, ^direct-superclasses(class)))
           ^ensure-slots-initialized-dynamically(class);
           #f
         else
           ^ensure-each-slot-initialized(class);
         end;
  end;
end function;

define function ^ensure-each-slot-initialized
    (class :: <&class>) => (well? :: <boolean>)
  block (return)
    let class-definition = model-definition(class);
    with-dependent-context ($compilation of class-definition)
      // If we're dynamic, pretend we have no slots
      unless (class-definition.form-dynamic?)
        local method fail ()
          ^ensure-slots-initialized-dynamically(class);
          return(#f);
        end method;
        let direct-slotds
          = map-as(<simple-object-vector>,
                   curry(make-slot-descriptor, fail, class),
                   form-slot-specs(class-definition));
        let inherited-slotds
          = map-as(<simple-object-vector>,
                   curry(make-inherited-slot-descriptor, fail, class),
                   form-inherited-slot-specs(class-definition));
        let initargds
          = map-as(<simple-object-vector>,
                   curry
                     (make-initialization-argument-descriptor, fail, class),
                   form-keyword-specs(class-definition));
        ^direct-slot-descriptors(class) 
           := mapped-model(direct-slotds);
        ^direct-inherited-slot-descriptors(class) 
           := mapped-model(inherited-slotds);
        ^direct-initialization-argument-descriptors(class) 
           := mapped-model(initargds);
      end unless;
      class.^slots-initialized-state := #"done";
      ^finalize-slots(class);
      #t
    end with-dependent-context;
  end block;
end function;

// The init method code generated here currently gets inlined as source
// into the default-initialize methods of subclasses, so we have to be
// careful not to insert objects that might not be available in other
// libraries. To that end, we now splice in the type expression to
// be re-folded in place rather than the model type.

// TODO: CORRECTNESS: Remember what's going on here when we get to computing
// the set of implicit exports, since the init expression and type are a bit
// like templates/inlineable functions.

// Just to recap, the init-expression is the expression associated
// with initializing the slot in whatever role: value, function, or
// expression (slot foo = expr()). The flags indicate the role
// the expression actually serves.

define function make-slot-init-model (init-expression,
				      init-expression? :: <boolean>,
				      init-value? :: <boolean>,
                                      type-expression,
				      type)
 => (init-data, init-value? :: <boolean>, init-evaluated? :: <boolean>)
  let init-function? = ~(init-value? | init-expression?);
  let (init-constant?, init-constant) =
    begin
      let init-constant
        = ^top-level-eval(init-expression, on-failure: $eval-failure);
      if (init-constant ~== $eval-failure)
        let init-constant = mapped-model(init-constant);
        let (ok?, init-constant) = inlineable?(init-constant);
        if (ok?)
          if (init-function? & instance?(init-constant, <&function>)
                & type == dylan-value(#"<object>"))
            // We can't trivially fold away init functions since the init-data
            // slot function must do the required type check. If we had a noddy
            // way to query function result type compatibility with a required
            // type, then we could use it here. But we'll leave the generic
            // optimizer to sort it out since the only penalty is at
            // compile-time, and people don't tend to use explicit 
            // init-function's much these days.
            values(#t, init-constant);
          elseif (~init-function? & instance?(type, <&type>))
            values(#t, init-constant);
          end;
        end;
      end;
    end;
  if (init-constant?)		// optimize common case
    values(init-constant, ~init-function?, #t)
  else
    with-expansion-source-location 
        (fragment-record(init-expression),
           fragment-source-position(init-expression))
      let init-form =
	if (init-expression?)
	  // implement init-expression as init-function
	  #{ () => (init-function)
	     method () => (init-value)
	       let (init-value :: ?type-expression) = ?init-expression;
	       init-value
	     end }
	elseif (init-value?)
          #{ () => (init-value)
	     let (init-value :: ?type-expression) = ?init-expression;
	     init-value }
	else
	  // init-function
	  if (type == dylan-value(#"<object>"))
	    #{ () => (init-function)
	       let init-function :: <function> = ?init-expression;
	       init-function }
	  else
	    // TODO: make sure this optimizes nicely
            #{ () => (init-function)
	       let init-function :: <function> = ?init-expression;
	       method () => (init-value)
		 let (init-value :: ?type-expression) = init-function();
		 init-value
	       end }
	  end
	end;
      values(convert-method-to-model-as
               (<&slot-initializer-method>, "slot-initializer", init-form),
	     init-value?,
	     #f)
    end
  end
end;

/*
define function lambda-return (lambda :: <&lambda>) => (res :: <return>)
  bind-return(body(lambda))
end function;
*/

define function lambda-returns-constant? (lambda :: <&lambda>)
 => (constant? :: <boolean>, constant)
  // also requires that there be no side-effects, so they don't get lost
  // TODO:  use singleton types when the typist does
  let values-c = next-computation(body(lambda));
  if (instance?(values-c, <values>) &
      instance?(next-computation(values-c), <return>))
    let values-temps = fixed-values(values-c);
    if (size(values-temps) > 0)
      let temp = values-temps[0];
      let (constant?, constant) = extractable-constant-value?(temp);
      if (constant?)
        extract-constant(temp);
        values(#t, constant)
      end
    end
  end
end;

define method optimize-slot-initializer (slot-descriptor :: <&slot-initial-value-descriptor>) => ()
  let init-model = ^init-data-slot(slot-descriptor);
  if (instance?(init-model, <&lambda>))
    with-dependent-context ($compilation of model-creator(init-model))
      ensure-method-dfm(init-model);
      // Type infer init-model
      type-initializer-method(init-model);
    end;
    // optimize it
    let init-model =
      if (^init-evaluated?(slot-descriptor))
	init-model
      else
	let (constant?, constant) = lambda-returns-constant?(init-model);
	if (constant?)
	  ^init-evaluated?(slot-descriptor) := #t;
	  ^init-data-slot(slot-descriptor) := constant;
	  constant
	else
	  init-model
	end
      end;
    if (instance?(init-model, <&lambda>) &
	  ^init-evaluated?(slot-descriptor) &
	  ~^init-value?(slot-descriptor))
      let (constant?, constant) = lambda-returns-constant?(init-model);
      if (constant?)
	^init-value?(slot-descriptor) := #t;
	^init-data-slot(slot-descriptor) := constant;
      end;
    end;
  end;
end;

define method optimize-slot-initializer (slot-descriptor :: <&repeated-slot-descriptor>) => ()
  next-method();
  optimize-slot-initializer(^size-slot-descriptor(slot-descriptor));
end;

define function optimize-slot-initializers (class :: <&class>) => ()
  do(optimize-slot-initializer, ^direct-slot-descriptors(class));
  do(optimize-slot-initializer, ^direct-inherited-slot-descriptors(class));
  do(optimize-slot-initializer, ^direct-initialization-argument-descriptors(class));
end;

define program-warning <dynamic-slot-type-expression>
  slot condition-form,
    init-keyword: form:;
  slot condition-type-expression,
    init-keyword: type-expression:;
  format-string
    "The slot type %= of %= can not be computed at compile-time -- \"<object>\" used instead.";
  format-arguments
    type-expression, form;
end program-warning;

define variable *fake-count* = 0;
define variable *fake-interval* = 4;

define function fake-failure? () 
  *fake-count* := *fake-count* + 1;
  modulo(*fake-count*, *fake-interval*) == 0;
  #f
end function;

define function get-slot-type-or-fail
    (slot-spec :: <slot-definition>, fail :: <function>) => (type :: <&type>)
  let type-expression = spec-type-expression(slot-spec);
  let type = ^top-level-eval(type-expression);
  if (type & instance?(type, <&type>))
    if (~fake-failure?())
      type
    else
      format-out("Fake failure: %s :: %s\n", slot-spec, type-expression);
      fail();
    end;
  else
    fail();
  end
end;

define function get-slot-accessor-or-fail
    (slot-spec :: type-union(<slot-definition>, <inherited-slot-spec>),
       spec-accessor :: <function>,
       fail :: <function>) 
 => (f :: false-or(<&function>))
  let name = spec-accessor(slot-spec);
  if (name)
    let (f, found?)
      = lookup-constant-model-object(#f, name, reference?: #f);
    if (found? & valid-accessor-function?(slot-spec, f))
      f
    else
      fail();
    end
  else
    #f
  end;
end;

define method valid-accessor-function? 
    (slot-spec :: <inherited-slot-spec>, f :: <object>) => (well? :: <boolean>)
  instance?(f, <&generic-function>);
end method;

define method valid-accessor-function? 
    (slot-spec :: <slot-definition>, f :: <object>) => (well? :: <boolean>)
  instance?(f, <&generic-function>)
    | (spec-virtual?(slot-spec) & instance?(f, <&function>))
end method;

define function get-slot-init-keyword
    (slot-spec :: <slot-keyword-initialization-spec>) 
 => (keyword :: false-or(<symbol>))
  // Parsing ensures that this fragment must be a literal keyword or #f.
  mapped-model(^top-level-eval(spec-init-keyword(slot-spec)))
end function;

define program-warning <dynamic-initialization-argument-type-expression>
  slot condition-form,
    init-keyword: form:;
  slot condition-type-expression,
    init-keyword: type-expression:;
  format-string
    "The initialization argument type %= of %= can not be computed at compile-time -- \"<object>\" used instead.";
  format-arguments
    type-expression, form;
end program-warning;

define function get-initialization-argument-type (slot-spec :: <init-arg-spec>)
 => (type :: <&type>)
  let type-expression = spec-type-expression(slot-spec);
  let type = ^top-level-eval(type-expression);
  if (type)
    type
  else
    note(<dynamic-initialization-argument-type-expression>,
	 // TODO: give a sharper source location
	 source-location: form-source-location(slot-spec),
	 form: slot-spec,
	 type-expression: type-expression);
    dylan-value(#"<object>")
  end
end;

define function make-slot-descriptor
    (fail :: <function>, class :: <&class>, slot-spec :: <slot-definition>)
 => (descriptor :: <&slot-descriptor>)
  let type = get-slot-type-or-fail(slot-spec, fail);
  let init-supplied? = spec-init-supplied?(slot-spec);
  let (init-data, init-value?, init-evaluated?) =
    if (init-supplied?)
      make-slot-init-model(spec-init-expression(slot-spec),
			   spec-init-expression?(slot-spec),
			   spec-init-value?(slot-spec),
                           spec-type-expression(slot-spec),
			   type);
    else
      values(#f, #f, #f)
    end if;
  let allocation = spec-allocation(slot-spec);
  let getter-model = get-slot-accessor-or-fail(slot-spec, spec-getter, fail);
  let setter-model = get-slot-accessor-or-fail(slot-spec, spec-setter, fail);
  ^make(^as-slot-descriptor-class(allocation),
	definition: slot-spec,
	owner: class, 
	type: type,
	getter: getter-model,
	setter: setter-model,
	init-keyword: get-slot-init-keyword(slot-spec),
	init-keyword-required?: spec-init-keyword-required?(slot-spec),
	init-supplied?: init-supplied?,
	init-value?: init-value?,
	init-evaluated?: init-evaluated?,
	init-data: mapped-model(init-data))
end function make-slot-descriptor;

define function make-inherited-slot-descriptor
    (fail :: <function>, class :: <&class>, slot-spec :: <inherited-slot-spec>)
 => (descriptor :: <&inherited-slot-descriptor>)
  let init-supplied? = spec-init-supplied?(slot-spec);
  let (init-data, init-value?, init-evaluated?) =
    if (init-supplied?)
      make-slot-init-model(spec-init-expression(slot-spec),
			   spec-init-expression?(slot-spec),
			   spec-init-value?(slot-spec),
                           #{ <object> },
			   dylan-value(#"<object>"));
    else
      values(#f, #f, #f)
    end if;
  ^make(<&inherited-slot-descriptor>,
        owner: class,
	getter: get-slot-accessor-or-fail(slot-spec, spec-getter, fail),
	init-supplied?: init-supplied?,
	init-value?: init-value?,
	init-evaluated?: init-evaluated?,
	init-data: mapped-model(init-data))
end function make-inherited-slot-descriptor;

define function make-initialization-argument-descriptor
    (fail :: <function>, class :: <&class>, slot-spec :: <init-arg-spec>)
 => (descriptor :: <&init-arg-descriptor>)
  // TODO: dynamic slot types
  let type = get-initialization-argument-type(slot-spec);
  let init-supplied? = spec-init-supplied?(slot-spec);
  let (init-data, init-value?, init-evaluated?) =
    if (init-supplied?)
      make-slot-init-model(spec-init-expression(slot-spec),
			   spec-init-expression?(slot-spec),
			   spec-init-value?(slot-spec),
                           spec-type-expression(slot-spec),
			   type);
    else
      values(#f, #f, #f)
    end if;
  ^make(<&init-arg-descriptor>,
        owner: class,
	type: type,
	init-keyword: get-slot-init-keyword(slot-spec),
	init-keyword-required?: spec-init-keyword-required?(slot-spec),
	init-supplied?: init-supplied?,
	init-value?: init-value?,
	init-evaluated?: init-evaluated?,
	init-data: mapped-model(init-data))
end function make-initialization-argument-descriptor;

//// Wrapper initialization

define method ^make-mm-wrapper (class :: <&class>)
  local method as-int (x :: <boolean>) if (x) 1 else 0 end end;
  local method as-raw (x :: <abstract-integer>)
	  if (instance?(x, <integer>))
	    ^make(<&raw-machine-word>, value: x)
	  else
	    let x :: <double-integer> = x;
	    ^make(<&raw-machine-word>, value: %double-integer-low(x))
	  end
	end;
  let repeated-slot = ^repeated-slot-descriptor(class);
  let slots      
    = remove(^instance-slot-descriptors(class),
             repeated-slot & ^size-slot-descriptor(repeated-slot));
  let raw-bits   = map(compose(raw-type?, ^slot-type), slots);
  let all-dylan? = ~any?(identity, raw-bits);
  let mixed?     = ~all-dylan?;
  let patterned? = mixed?;
  local method number-words (slots)
	  // HACK: SHOULD REDUCE + TYPE-SIZES OF SLOTS
	  if (class == dylan-value(#"<double-float>"))
	    2
	  else
	    size(slots)
	  end if
	end method;
  let fixed-part = logior(logior(ash(number-words(slots), 2), 
				 ash(as-int(mixed?), 1)),
			  as-int(all-dylan?));
  let variable-part
    = if (repeated-slot)
        let repeated-type
          = repeated-slot.^slot-type;
        let byte?     
          = repeated-representation-byte?(repeated-type);
        let repeated-size
          = repeated-representation-size(repeated-type);
        let non-word? 
          = repeated-size ~= word-size();
        let untraceable? 
          = raw-type?(repeated-type) 
              | raw-repeated-representation?(repeated-type);
        let stretchy? 
          = ^subtype?(class, dylan-value(#"<stretchy-vector>"));
        if (untraceable? & ~non-word?)
          if (stretchy?)
            1                                 // stretchy non-traceable
          else
            0                                 // non-traceable
          end if 
        else                                  // traceable
          if (non-word?)
            let bias 
	      = if (class == dylan-value(#"<byte-string>"))
		  #x00010000;     // null-terminated
		else
		  0
		end if;
            let element-size 
	      = truncate(logn(repeated-size * 8, 2)); // log2(number-of-bits) 
            if (stretchy?)
              logior(ash(element-size, 3), 5, bias) // stretchy non-word
            else
              logior(ash(element-size, 3), 4, bias) // non-word
            end if 
          else
            if (stretchy?)
              3                               // stretchy traceable
            else
              2                               // traceable
            end if 
          end if
        end if;
      else
        7
      end if;
  let variable-part = logior(variable-part, #x02000000); // VERSION 2
  let word-size = word-size() * 8; // word size in bits
  local method word-pattern (start :: <integer>, stop :: <integer>)
          iterate loop (i :: <integer> = stop - 1, pattern = 0)
            if (i < start)
	      pattern
	    else
	      loop(i - 1, generic/logior(generic/ash(pattern, 1), as-int(~raw-bits[i])));
            end if;
  	  end iterate;
        end method;
  let patterns
    = if (patterned?)
	collecting ()
	  iterate loop (i = word-size)
	    if (i > raw-bits.size)
	      collect(word-pattern(i - word-size, raw-bits.size))
	    else
	      collect(word-pattern(i - word-size, i));
	      loop(i + word-size);
	    end if;
          end iterate;
        end collecting
      else
        #()
      end if;
  let iclass :: <&implementation-class> = ^class-implementation-class(class);
  let mask :: <integer> = 0;
  for (sup in ^all-superclasses(iclass))
    mask := logior(mask, ^class-subtype-bit(sup))
  end for;
  let mm-wrapper
    = ^make(<&mm-wrapper>, 
	    implementation-class: iclass,
            subtype-mask:         mask,
	    fixed-part:           as-raw(fixed-part),
	    variable-part:        as-raw(variable-part),
	    number-patterns:      patterns.size,
            patterns:             map-as(<vector>, as-raw, patterns));
  // format-out("WRAPPER         %=\n", class);
  // format-out("FIXED-PART      %x\n", fixed-part);
  // format-out("VARIABLE-PART   %x\n", variable-part);
  // format-out("NUMBER PATTERNS %d\n", patterns.size);
  // for (pattern in patterns)
  //   format-out("PATTERN         %x\n", pattern);
  // end for;

  mm-wrapper
end method;

define function ^finalize-slots (class :: <&class>)
  // TODO: Do the fix and continue for this stuff properly within the
  // framework when the framework exists.
  if (^check-slot-inheritance(class))
    ^compute-slot-descriptors(class);
    ^update-class-slots!(class);
  end;
  // TODO: Support the memory management wrapper stuff.
  let mm-wrapper :: <&mm-wrapper> = ^make-mm-wrapper (class);
  class.^class-mm-wrapper := mm-wrapper;
  
  // ^make-mm-wrapper (class);
  class
end function ^finalize-slots;

define serious-program-warning <duplicated-direct-slot-definitions>
  slot condition-defining-class,
    init-keyword: defining-class:;
  slot condition-slots,
    required-init-keyword: slots:;
  format-string "The directly-defined slots %= of %= share an accessor.";
  format-arguments slots, defining-class;
end;

define serious-program-warning <clashing-inherited-slot-definitions>
  slot condition-inheriting-class,
    init-keyword: inheriting-class:;
  slot condition-slots,
    required-init-keyword: slots:;
  slot condition-owners,
    required-init-keyword: owners:;
  format-string 
    "The distinct inherited slots of %=, %= with respective owners %=, "
    "share an accessor and may not be mixed in the same class.";
  format-arguments inheriting-class, slots, owners;
end;

define serious-program-warning <clashing-direct-and-inherited-slot-definitions>
  slot condition-inheriting-class,
    init-keyword: inheriting-class:;
  slot condition-direct-slot,
    required-init-keyword: direct-slot:;
  slot condition-inherited-slot,
    required-init-keyword: inherited-slot:;
  slot condition-inherited-owner,
    required-init-keyword: inherited-owner:;
  format-string 
    "The directly-defined slot %= of %= shares an accessor with %= "
    "inherited from %=.";
  format-arguments 
    direct-slot, inheriting-class, inherited-slot, inherited-owner;
end;

define function ^check-slot-inheritance 
    (class :: <&class>) => (ok? :: <boolean>)
  // TODO: Do the fix and continue for this stuff properly within the
  // framework when the framework exists.
  let ok? = #t;
  // Are there any problems among just the directly-defined slots?
  let direct = ^direct-slot-descriptors(class);
  gts-debug("bug1318", "class(direct)=%=, class(dsds)=%=.\n", 
    object-class(direct), object-class(^direct-slot-descriptors(class)));
  let duplicates = collect-duplicates(direct, test: ^accessor=);
  for (duplicate-set in duplicates)
    note(<duplicated-direct-slot-definitions>,
         source-location: model-source-location(class),
         context-id: model-context-id(class),
         defining-class: model-id(class),
         slots: model-ids(duplicate-set));
    ok? := #f;
  end;
  // Attempt to "fix" and continue if necessary...
  gts-debug("bug1318", "class=%=, direct=%=, dupes=%=.\n", class, direct, duplicates);
  let direct 
    = if (empty?(duplicates))
        direct;
      else 
        // (gts,98jan12) remove non-first dupes instead of 1st.
        //  ** because 1st accessor method will be chosen by default.
        let new-direct = 
          iterate loop (todo = as(<list>, direct), done = list(), new-direct = list())
            if (empty?(todo))
              new-direct;
            else
              let this-sd = head(todo);
              if (member?(this-sd, done, test: ^accessor=))
                gts-debug("bug1318", "dupe: %=.\n", this-sd);

                // nuke (pointers to) the methods for setters and getters, too
                // 1. find the GF for the slot-setter,
                // 2. find the method on the GF with ^method-slot-descriptor
                //    == this slot-descriptor.
                // 3. nuke that method from GF's list.
                // let mod = form-model-object(this-sd);
                // break("mod = %=", mod);
                // tried and didn't work:
                //    this-sd.model-object-{g,s}etter := #f;
                //    this-sd.^slot-{g,s}etter := #f;
                //    this-sd.model-definition.form-model := #f;

                let this-getter-def 
                  = this-sd.model-definition.form-getter-definition;

                if (this-getter-def)
                  this-getter-def.form-model := #f;
                  let this-slot-getter-method
                    = find-element
                        (%generic-function-methods(^slot-getter(this-sd)), 
                         method(m) ^method-slot-descriptor(m) == this-sd; end);
                  this-sd.^slot-getter.%generic-function-methods
                    := remove(this-sd.^slot-getter.%generic-function-methods, 
                              this-slot-getter-method);
                  gts-debug("bug1318", "getter methods now = %=.\n",
                            this-sd.^slot-getter.^generic-function-methods);
                end;

                let this-setter-def
                  = this-sd.model-definition.form-setter-definition;

                if (this-setter-def)
                  this-setter-def.form-model := #f; 
                  let this-slot-setter-method
                    = find-element
                        (%generic-function-methods(^slot-setter(this-sd)), 
                         method(m) ^method-slot-descriptor(m) == this-sd; end);
                  this-sd.^slot-setter.%generic-function-methods
                    := remove(this-sd.^slot-setter.%generic-function-methods, 
                              this-slot-setter-method);
                  gts-debug("bug1318", "setter methods now = %=.\n",
                            this-sd.^slot-setter.^generic-function-methods);
                end;

                loop (tail(todo), done, new-direct);
              else
                loop (tail(todo), pair(this-sd, done), 
                      pair(this-sd, new-direct));
              end if;
            end if;
          end iterate;
        ^direct-slot-descriptors(class) 
           := as(<vector>, reverse(new-direct)); 
                 // remove-duplicates(direct, test: ^accessor=)
      end if;
  gts-debug("bug1318", "\tdirect now = %=.\n", direct);
  // Among the inherited slots?
  let inherited = ^inherited-direct-slot-descriptors(class);
  let duplicates = collect-duplicates(inherited, test: ^accessor=);
  for (duplicate-set in duplicates)
    note(<clashing-inherited-slot-definitions>,
         source-location: model-source-location(class),
         context-id: model-context-id(class),
         inheriting-class: model-id(class),
         slots: model-ids(duplicate-set),
         owners: map(compose(model-id, ^slot-owner), duplicate-set));
    ok? := #f;
  end;
  // Between the inherited slots and the directly-defined slots?
  let duplicates 
    = collect-duplicates(concatenate(direct, inherited), test: ^accessor=);
  for (duplicate-set in duplicates)
    let direct = duplicate-set.head;
    if (direct.^slot-owner == class)
      for (inherited in duplicate-set.tail)
        note(<clashing-direct-and-inherited-slot-definitions>,
             source-location: model-source-location(class),
             context-id: model-context-id(class),
             inheriting-class: model-id(class),
             direct-slot: model-id(direct),
             inherited-slot: model-id(inherited),
             inherited-owner: model-id(inherited.^slot-owner));
        ok? := #f;
      end;
    end;
  end;
  ok?
end function ^check-slot-inheritance;

define function ^inherited-direct-slot-descriptors 
    (class :: <&class>) => (descriptors :: <sequence>)
  reduce(method (descriptors, class)
           concatenate(^direct-slot-descriptors(class), descriptors)
         end,
         #(),
         tail(^all-superclasses(class)));
end function ^inherited-direct-slot-descriptors;

// Within each duplicate sequence in the returned sequence of sequences,
// the original order within the given sequence is maintained.

define function collect-duplicates 
    (seq :: <sequence>, #key test = \==) => (seq-of-seqs :: <sequence>)
  let seq-of-seqs = #();
  local method seen? (elt) 
    member?(elt, seq-of-seqs, 
            test: method (elt, seq) test(elt, seq.first) end);
  end;
  let n-elts = size(seq);
  for (i from 0 below n-elts - 1)
    let elt = seq[i];
    if (~seen?(elt))
      collecting (duplicates)
        for (j from i + 1 below n-elts)
          let test-elt = seq[j];
          if (test(elt, test-elt))
            collect-into(duplicates, test-elt);
          end;
        end;
        let duplicates = collected(duplicates);
        if (~empty?(duplicates))
          seq-of-seqs := pair(pair(elt, duplicates), seq-of-seqs);
        end;
      end;
    end;
  end;
  seq-of-seqs
end function collect-duplicates;

//// Inheritance validity checking.

define serious-program-warning <inheritance-violation>
  slot condition-library-name,
    init-keyword: library-name:;
  slot condition-superclass-name,
    init-keyword: superclass-name:;
  slot condition-subclass-name,
    init-keyword: subclass-name:;
  format-arguments subclass-name, superclass-name;
end serious-program-warning;

define method note-inheritance-violation (error-class, class, super)
  note(error-class,
       source-location: model-source-location(class),
       subclass-name:   model-variable-name(class),
       superclass-name: model-variable-name(super),
       library-name:    library-description-emit-name(model-library(super)));
end method;

define serious-program-warning <subclass-of-a-sealed-imported-class>
    (<inheritance-violation>)
  format-string "The class %= cannot be a subclass of %= because this "
                "superclass was defined sealed in %=.";
  format-arguments subclass-name, superclass-name, library-name;
end serious-program-warning;

define serious-program-warning <abstract-subclass-of-a-concrete-class>
    (<inheritance-violation>)
  format-string "Illegal abstract subclass %= of a concrete superclass %=.";
  //format-string "The abstract class %= cannot be a subclass of the concrete "
  //              "class %=.";
end serious-program-warning;

define serious-program-warning
  <subclass-combines-exclusive-primary-superclasses>
    (<inheritance-violation>)
  slot condition-other-superclass-name,
    init-keyword: other-superclass-name:;
  format-string "The class %= cannot be a subclass of both the exclusive "
                "primary superclasses %= and %=.";
  format-arguments subclass-name, superclass-name, other-superclass-name;
end serious-program-warning;

define method ^check-inheritance (class :: <&class>) => ()
  // Simple sealing violations: can't subclass sealed class in another library.
  for (super in ^direct-superclasses(class))
    if (^class-sealed?(super) & model-library(super) ~== model-library(class))
      note-inheritance-violation
        (<subclass-of-a-sealed-imported-class>, class, super)
    end
  end;
  // Abstract/concrete violations: Concrete classes can't have abstract kids.
  if (^class-abstract?(class))
    for (super in ^direct-superclasses(class))
      if (~ ^class-abstract?(super))
        note-inheritance-violation
          (<abstract-subclass-of-a-concrete-class>, class, super)
      end
    end
  end;
  // Primary/free violations: all primary superclasses must be in 
  // a mutual subtype/supertype relation, i.e., 2 incomparable primary
  // classes can never be mixed.  (I.e., primaries are singly inherited.)
  // 
  // Assume all the superclasses of class satisfy the primary/free constraint.
  // The _primary_ superclasses are an intercomparable subsequence of the CPL.
  // Thus their subtype DAG is in fact a chain.  Let p1 and p2 be elements
  // of that chain.  We must enforce that either p1 < p2 or p2 < p1.  WLOG,
  // assume that p1 < p2 comes up.  Then the entire chain above p2 is already
  // in p1's CPL, so we're guaranteed that p1 < (entire chain above p2).  Since 
  // p1 was chosen arbitrarily, the chain above p1 is well-ordered, too, by
  // a symmetrical argument.
  //
  // Consequence: we only have to check the least primary classes (bottoms of 
  // the chains) for intercomparability.  So look @ least primary of each super.
  //       -- sgr 16-Jul-96
  for (rest-prims // List of least primary superclasses, where present
         = collecting ()
	     for(super in ^direct-superclasses(class))
	       let lps = ^least-primary-superclass(super);
	       if (lps)
		 collect(lps)
	       end
	     end
           end
	 then tail(rest-prims),
       until: empty?(rest-prims))
    let test-class = head(rest-prims);
    for (problem-class in tail(rest-prims)) // Exploit symmetry of test
      unless (^subtype?(problem-class, test-class   ) |
	      ^subtype?(test-class,    problem-class))
	note(<subclass-combines-exclusive-primary-superclasses>,
             source-location: model-source-location(class),
	     subclass-name:   model-variable-name(class),
	     superclass-name: model-variable-name(test-class),
	     other-superclass-name: model-variable-name(problem-class),
             library-name:    
               library-description-emit-name(model-library(problem-class)));
      end
    end
  end
end;

//// CPL computation.

define function ^compute-class-precedence-list (c :: <&class>)
    => cpl :: <list>;
  let c-direct-superclasses = as(<list>, ^direct-superclasses(c));

  local method merge-lists 
      (partial-cpl :: <list>, remaining-lists :: <list>)
    // The partial-cpl is in reverse order at this point.
    if (every?(empty?, remaining-lists))
      reverse!(partial-cpl)
    else
      local 
        method candidate (s :: <&class>) 
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
        // We are trying to build a class with an inconsistent CPL.
        // Find two classes, and two witnesses, such that
        // witness 1 < witness 2 in CPL of class 1 and reverse in class 2
        let (witness1, witness2, index-of-class1, index-of-class2) =
          block (return)
            for (i from 0 below remaining-lists.size)
              unless (empty?(remaining-lists[i]))
                let candidate = remaining-lists[i].head;
                let constraints = remaining-lists[i].tail;
                for (j from i + 1 below remaining-lists.size)
                  unless (empty?(remaining-lists[j]))
                    if ( member?(remaining-lists[j].head, constraints) 
                       & member?(candidate, remaining-lists[j].tail) )
                      return (candidate, remaining-lists[j].head, i, j)
                    end if
                  end unless
                end for
              end unless
            end for;
          end block;

        // Signal an error explaining what has gone wrong.
        let class1 = if (index-of-class1 = 0) 
                       c 
                     else 
                       c-direct-superclasses[index-of-class1 - 1] 
                     end;
        let class2 = c-direct-superclasses[index-of-class2 - 1];
        note-CPL-inconsistency(
          c,
          class1,
          class2, 
          witness1, witness2);
        // gts,98apr03:  a naive attempt to keep going
        // remove a superclass,
        c-direct-superclasses := remove(c-direct-superclasses, class2);
        // and try again
        merge-lists(list(c), 
                    concatenate(map(^all-superclasses, c-direct-superclasses),
                                list(c-direct-superclasses)));
      end
    end
  end;
  
  let c3 = merge-lists(list(c),
                       concatenate(map(^all-superclasses, c-direct-superclasses),
                                   list(c-direct-superclasses)));
  let old = ^compute-class-precedence-list-old(c);
  unless (every?(\=, c3, old))
    let name = compose(fragment-identifier, model-variable-name);
    note(<cpl-differ>,
         source-location: c.model-source-location,
         class-name: c.name,
         dylan-linearization: map(name, old),
         cthree-linearization: map(name, c3))
  end;
  c3;
end function ^compute-class-precedence-list;

define serious-program-warning <cpl-differ>
  slot class-name, init-keyword: class-name:;
  slot dylan-linearization, init-keyword: dylan-linearization:;
  slot cthree-linearization, init-keyword: cthree-linearization:;
  format-string "The class precedence list of %= differ, Dylan: %=; C3: %=";
  format-arguments class-name, dylan-linearization, cthree-linearization;
end;

define function ^compute-class-precedence-list-old (c :: <&class>)
    => cpl :: <list>;
  let c-direct-superclasses = as(<list>, ^direct-superclasses(c));

  local method merge-lists 
      (partial-cpl :: <list>, remaining-lists :: <list>)
    // The partial-cpl is in reverse order at this point.
    if (every?(empty?, remaining-lists))
      reverse!(partial-cpl)
    else
      local 
        method unconstrained-class (s :: <&class>) 
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

        method unconstrained-class-in-superclasses (c :: <&class>)
          any?(unconstrained-class, ^direct-superclasses(c))
        end method;
          
      let next = any?(unconstrained-class-in-superclasses, partial-cpl);
          
      if (next)
        local method remove-next (l :: <list>)
          if (head(l) == next) tail(l) else l end
        end method;
        merge-lists(pair (next, partial-cpl),
                    map-into(remaining-lists, remove-next, remaining-lists))
      else
        // We are trying to build a class with an inconsistent CPL.
        // Find two classes, and two witnesses, such that
        // witness 1 < witness 2 in CPL of class 1 and reverse in class 2
        let (witness1, witness2, index-of-class1, index-of-class2) =
          block (return)
            for (i from 0 below remaining-lists.size)
              unless (empty?(remaining-lists[i]))
                let candidate = remaining-lists[i].head;
                let constraints = remaining-lists[i].tail;
                for (j from i + 1 below remaining-lists.size)
                  unless (empty?(remaining-lists[j]))
                    if ( member?(remaining-lists[j].head, constraints) 
                       & member?(candidate, remaining-lists[j].tail) )
                      return (candidate, remaining-lists[j].head, i, j)
                    end if
                  end unless
                end for
              end unless
            end for;
          end block;

        // Signal an error explaining what has gone wrong.
        let class1 = if (index-of-class1 = 0) 
                       c 
                     else 
                       c-direct-superclasses[index-of-class1 - 1] 
                     end;
        let class2 = c-direct-superclasses[index-of-class2 - 1];
        note-CPL-inconsistency(
          c,
          class1,
          class2, 
          witness1, witness2);
        // gts,98apr03:  a naive attempt to keep going
        // remove a superclass,
        c-direct-superclasses := remove(c-direct-superclasses, class2);
        // and try again
        merge-lists(list(c), 
            pair(c-direct-superclasses,
                   map(^all-superclasses, c-direct-superclasses)));
      end
    end
  end;
  
  merge-lists(list(c),
              pair(c-direct-superclasses,
                   map(^all-superclasses, c-direct-superclasses)));
end function ^compute-class-precedence-list-old;

define serious-program-warning <inconsistent-CPL>
  slot class-1-name,   init-keyword: class-1:;
  slot class-2-name,   init-keyword: class-2:;
  slot witness-1-name, init-keyword: witness-1:;
  slot witness-2-name, init-keyword: witness-2:;
  format-string "The class precedence list of %s makes %s precede %s, "
                "which conflicts with the CPL of %s";
  format-arguments class-1, witness-1, witness-2, class-2;
end;


define method note-CPL-inconsistency(c, class1, class2, witness1, witness2)
  // witness1 < witness2 in CPL of class1
  // witness1 > witness2 in CPL of class2

  let name = compose(fragment-identifier, model-variable-name);

  note(<inconsistent-CPL>,
       source-location: c.model-source-location,
       class-1: class1.name,
       class-2: class2.name,
       witness-1: witness1.name,
       witness-2: witness2.name,
       subnotes: vector(
                 explain-precedes(witness1, witness2, class1),
                 explain-precedes(witness2, witness1, class2)))
end;

// Predicate to check if c1 precedes c2 in the local precedence order of in
define method precedes?(c1, c2, in)
  let c1-found? = #f;
  block (return)
    for (s in in.^direct-superclasses)
      if (s == c1) c1-found? := #t
      elseif (s == c2) return (c1-found?)
      end
    finally #f
    end
  end
end method precedes?;

define program-note <precedes-explanation>
  slot class-1-name,  init-keyword: class-1:, init-value: #f;
  slot class-2-name,  init-keyword: class-2:, init-value: #f;
  slot class-3-name,  init-keyword: class-3:, init-value: #f;
end;

define program-note <simple-subclass-explanation>(<precedes-explanation>)
  format-string "%s precedes %s because it is a direct subclass of %s";
  format-arguments class-1, class-2, class-2 again;
end;

define program-note <subclass-explanation-1>(<precedes-explanation>)
  format-string "%s precedes %s because %s is a direct subclass of %s "
                "and %s precedes %s";
  format-arguments 
    class-1, class-2, class-1 again, class-3, class-3 again, class-2 again;
end;

define program-note <subclass-explanation-2>(<precedes-explanation>)
  slot class-4-name,  init-keyword: class-3:;
  format-string "%s precedes %s in the CPL of %s "
                "because %s is a direct subclass of %s "
                "and %s precedes %s in %s";
  format-arguments class-1, class-2, class-3, 
                   class-3 again, class-4, class-1 again, class-2 again,
                   class-4 again;
end;

define program-note <local-pl-explanation>(<precedes-explanation>)
  format-string "%s precedes %s in the CPL of %s because of the "
                "local precedence order of %s";
  format-arguments class-1, class-2, class-3, class-3 again;
end;

define program-note <transitivity-explanation>(<precedes-explanation>)
  format-string "%s precedes %s in the CPL of %s by transitivity";
  format-arguments class-1, class-2, class-3;
end;

define method explain-precedes(c1, c2, in)        
  let name = compose(fragment-identifier, model-variable-name);

  let direct-supers = ^direct-superclasses(in);

  block (return)
    if (c1 == in)
      if (member?(c2, direct-supers))
        return(make(<simple-subclass-explanation>,
                    class-1: c1.name, class-2: c2.name))
      else
        for (s in direct-supers)
          if (member?(c2, s.^all-superclasses))
            return(make(<subclass-explanation-1>,
                     class-1: c1.name, class-2: c2.name, class-3: s.name,
                     subnotes: list(explain-precedes(s, c2, s))))
          end
        end
      end
    end;

    if (member?(c1, direct-supers) & member?(c2, direct-supers))
      return(make(<local-pl-explanation>,
                  class-1: c1.name, class-2: c2.name, class-3: in.name));
    end;

    for (s in direct-supers)
      if (precedes?(c1, c2, s))
        return(make(<subclass-explanation-2>,
                    class-1: c1.name, class-2: c2.name, 
                    class-2: in.name, class-4: s.name,
                    subnotes: list(explain-precedes(c1, c2, s))))
      end
    end;

    // If we get here it means we need to search for a path from c1 to c2
    let path = cpl-search(c1, c2, in);
    make(<transitivity-explanation>,
         class-1: c1.name, class-2: c2.name, class-3: in.name,
         subnotes: map(curry(apply, explain-precedes), path))
  end
end;

define method cpl-search(c1, c2, in)
  local method edges(c)
    // There is an edge from c to all it's direct superclasses plus an edge
    // to any class d such that c precedes d in the local precedence list of
    // some other class, i.e. we have define class C (... c ... d ...)

    // The edges are stored as triples of the form (from, to, in)

    let edge-list = map-as(<list>, 
                           method (sc) vector(c, sc, c) end,
                           c.^direct-superclasses);

    // Here I should just look at all subclasses of c that are reachable from 
    // in, but ^direct-subclasses sometimes complains of cycles in the CPL for
    // some reason.  So we do it less efficiently for now.
    for (sc in in.^all-superclasses)
      if (member?(c1, sc.^direct-superclasses))
        for (cc in sc.^direct-superclasses, 
             c-found = #f then c-found | cc == c)
          if (c-found) edge-list := pair(
                vector(c, cc, sc), edge-list) end;
        end
      end
    end;

    edge-list 
  end;

  local method search(el)
    let h = el.head;
    if (h.head[1] == c2)
      reverse(h)
    else
      let e = edges(h.head[1]);
      search(concatenate(el.tail, map(rcurry(pair, h), e)))
    end
  end;

  search(map(list, edges(c1)));
end;


//// Slot handling.

define method ^update-class-slots! (class :: <&class>) => ()
  let storage = make(<simple-object-vector>, size: size(^class-slot-descriptors(class)));
  class.^class-slot-storage 
    := mapped-model(storage)
end method ^update-class-slots!;

define function ^compute-defaulted-initialization-arguments (class :: <&class>)
 => ()
  let keywords = #();
  let required-keywords = #();
  let all-init-value? = #t;
  let all-evaluated? = #t;
  let init-values = #();
  for (c in ^all-superclasses(class))
    for (descriptor in ^direct-initialization-argument-descriptors(c))
      let keyword = descriptor.^init-keyword;
      if (member?(keyword, required-keywords))
	// don't look at it
      elseif (descriptor.^init-keyword-required?)
	required-keywords := pair(keyword, required-keywords);
      elseif (descriptor.^init-supplied? & ~member?(keyword, keywords))
	keywords := pair(keyword, keywords);
	if (descriptor.^init-value?)
	  if (descriptor.^init-evaluated?)
            let (inlineable?, init-value)
              = inlineable?(^init-data-slot(descriptor));
            if (inlineable?)
  	      init-values := pair(init-value, init-values);
            else
  	      all-evaluated? := #f;
	    end
	  else
	    all-evaluated? := #f;
	  end
	else
	  all-init-value? := #f;
	end;
      end;
    end;
  end;
  let vector-size = size(keywords) * 2;
  class.^defaulted-initialization-arguments-slot :=
    if (all-init-value?)
      if (all-evaluated?)
	let result = make(<simple-object-vector>, size: vector-size);
	for (i from 0 by 2,
	     keyword in keywords,
	     init-value in init-values)
	  result[i] := keyword;
	  result[i + 1] := init-value;
	end;
	result
      else
	// flag to initialize the vector first time
	- vector-size
      end
    else
      // tell the runtime how big to make the vector, to save consing
      vector-size
    end;
end function ^compute-defaulted-initialization-arguments;

define method ^compute-slot-descriptors (class :: <&class>)
  let all-slots :: <stretchy-object-vector> = make(<stretchy-vector>);
  let icount :: <integer> = 0;
  let ccount :: <integer> = 0;
  let repeater = #f;
  let repeater-size = #f;
  local method collect-superclass-slots (superclasses :: <list>) => ()
	  unless (empty?(superclasses))
	    // first get superclass slots
	    collect-superclass-slots(tail(superclasses));
	    let c :: <&class> = head(superclasses);
	    for (sd :: <&slot-descriptor> in ^direct-slot-descriptors(c))
	      block (duplicate-slot)
		let g = ^slot-getter(sd);
		for (osd :: <&slot-descriptor> in all-slots)
		  if (g == ^slot-getter(osd)) duplicate-slot() end if
		end for;
		all-slots := add!(all-slots, sd);
		select (^slot-allocation(sd) by \==)
		  #"instance" => icount := icount + 1;
		  #"each-subclass", #"class" => ccount := ccount + 1;
		  #"virtual" => ;
		  #"repeated" =>
		    if (repeater)
		      error("Multiple repeated slots %= and %= in %=", repeater, sd, class)
		    else
		      repeater := sd;
		      repeater-size := ^size-slot-descriptor(sd);
		      icount := icount + 1     // one for the size slot
		    end if;
		end select
	      end block
	    end for
	  end unless
	end method;
  collect-superclass-slots(^all-superclasses(class));
  let ivector :: <simple-object-vector> =
    make(<simple-object-vector>, size: icount, fill: #f);
  let cvector :: <simple-object-vector> =
    make(<simple-object-vector>, size: ccount, fill: #f);
  let first-primary = begin
                        local method loop (l :: <list>)
                                if (l == #())
                                  #f
                                else
                                  let c :: <&class> = head(l);
                                  if (^class-primary?(c))
                                    c
                                  else
                                    loop(tail(l))
                                  end if
                                end if
                              end method;
                        loop(tail(^all-superclasses(class)))
                      end;
  when (first-primary)
    let merge-vectors
      = method (into-vec :: <simple-object-vector>, from-vec :: <simple-object-vector>)
          for (i from 0 below size(from-vec))
            let supsd = ^vector-element(from-vec, i);
            when (supsd)
              let sd = ^vector-element(into-vec, i);
              if (sd)
                unless (^getter=(supsd, sd))
                  error("Class %= has slot conflict with %= and %=", sd, supsd)
                end unless
              else
                ^vector-element(into-vec, i) := supsd
              end if
            end when
          end for
        end method;
    merge-vectors(ivector, ^instance-slot-descriptors(first-primary));
    merge-vectors(cvector, ^class-slot-descriptors(first-primary))
  end when;
  local method allocate-superclass-slots (superclasses :: <list>) => ()
	  unless (empty?(superclasses))
	    // first allocate slots for superclasses
	    allocate-superclass-slots(tail(superclasses));
	    let c :: <&class> = head(superclasses);
	    unless (first-primary & ^subtype?(first-primary, c))
	      let position-slot
		= method (sd, vec :: <simple-object-vector>)
		    let n :: <integer> = size(vec);
		    local method loop (i :: <integer>)
			    if (i == n)
			      error("Bug - ran out of space for %= in %=", sd, class)
			    else
			      if (^vector-element(vec, i))
				loop(i + 1)
			      else
				^vector-element(vec, i) := sd
			      end if
			    end if
			  end method;
		    loop(0)
		  end method;
	      for (sd in ^direct-slot-descriptors(c))
		select (^slot-allocation(sd) by \==)
		  #"instance"  =>
		    position-slot(sd, ivector);
		  #"each-subclass", #"class" => 
		    position-slot(sd, cvector);
		  #"virtual" => ;
		  #"repeated" => 
		    let i :: <integer> = icount - 1;
		    if (~(^vector-element(ivector, i)))
		      ^vector-element(ivector, i) := repeater-size
		    else
		      error("Bug - canonical slot for repeating size in use already")
		    end if;
		end select
	      end for
	    end unless
	  end unless
	end method;
  allocate-superclass-slots(^all-superclasses(class));
  class.^slot-descriptors :=
    mapped-model(if (all-slots = ivector)
		   // often, all slots are instance slots
		   ivector
		 elseif (all-slots = cvector)
		   // or maybe they're all class slots
		   cvector
		 else
		   // oh, well -- make a new vector
		   as(<simple-object-vector>, all-slots)
		 end);
  class.^instance-slot-descriptors := mapped-model(ivector);
  class.^class-slot-descriptors := mapped-model(cvector);
  class.^repeated-slot-descriptor := repeater;
  class.^instance-storage-size := icount;
  values()
end method ^compute-slot-descriptors;

define method ^getter=
    (descriptor-1 :: <&slot-descriptor>, descriptor-2 :: <&slot-descriptor>)
  descriptor-1.^slot-getter == descriptor-2.^slot-getter
end method ^getter=;

define method ^setter=
    (descriptor-1 :: <&slot-descriptor>, descriptor-2 :: <&slot-descriptor>)
  descriptor-1.^slot-setter 
    & descriptor-2.^slot-setter
    & descriptor-1.^slot-setter == descriptor-2.^slot-setter
end method ^setter=;

define method ^accessor=
    (descriptor-1 :: <&slot-descriptor>, descriptor-2 :: <&slot-descriptor>)
  ^getter=(descriptor-1, descriptor-2)
    | ^setter=(descriptor-1, descriptor-2)
end method ^accessor=;

//// Slot initialization.

define compiler-sideways method ^initialize-slot-descriptor
    (slot :: <&slot-descriptor>, 
       #key owner, #all-keys)
  // Install slot descriptors into accessor methods.
  install-slot-descriptor-in-accessor-methods(slot, owner);
  // Install compile stage versions of the slot's accessors if 
  // they exist.
  slot.model-object-getter := compute-compile-stage-getter(slot);
  slot.model-object-setter := compute-compile-stage-setter(slot);
end method;

define method ^as-slot-descriptor-class (allocation)
  error("Unrecognized allocation %=", allocation)
end method;

//// Slot access.

// Convenient slot value iteration for the back-end.

define method do-instance-slot-values (f, o)
  let class = &object-class(o);
  ^ensure-slots-initialized(class);
  for (slotd in ^instance-slot-descriptors(class))
    do-instance-slot-value(f, slotd, o);
  end;
  let repeated-slotd = class.^repeated-slot-descriptor;
  if (repeated-slotd)
    do-instance-slot-value(f, repeated-slotd, o)
  end if;
end method;

define method do-instance-slot-value 
    (f, slotd :: <&slot-descriptor>, o) => ()
  f(slotd, ^slot-value(o, slotd));
end method;

define method do-instance-slot-value 
    (f, slotd :: <&repeated-slot-descriptor>, o) => ()
  let size = ^slot-value(o, ^size-slot-descriptor(slotd));
  for (i from 0 below size)
    f(slotd, ^repeated-slot-value(o, slotd, i));
  end;
end method;

define macro for-instance-slot-value
  { for-instance-slot-value (?:variable in ?:expression) ?:body end }
    => { do-instance-slot-values
           (method (_dummy, ?variable) ?body end, ?expression) }
  { for-instance-slot-value 
        (?:variable described-by ?desc:variable in ?:expression)
      ?:body
    end }
    => { do-instance-slot-values
           (method (?desc, ?variable) ?body end, ?expression) }
end macro;

//// Layout walking.

define inline method do-layout-fixed-slot-values (f, o)
  let class = &object-class(o);
  ^ensure-slots-initialized(class);
  for (slotd in ^instance-slot-descriptors(class))
    do-layout-fixed-slot-value(f, slotd, o);
  end;
  let repeated-slotd = class.^repeated-slot-descriptor;
  if (repeated-slotd)
    let size-slotd = ^size-slot-descriptor(repeated-slotd);
    f(size-slotd, ^slot-value(o, size-slotd))
  end if;
end method;

define method do-layout-fixed-slot-value
    (f, slotd :: <&slot-descriptor>, o) => ()
  f(slotd, ^slot-value(o, slotd));
end method;

define method do-layout-fixed-slot-value
    (f, slotd :: <&virtual-slot-descriptor>, o) => ()
end method;

define inline method do-layout-repeated-slot-values (f, o)
  let class = &object-class(o);
  ^ensure-slots-initialized(class);
  let slotd = class.^repeated-slot-descriptor;
  if (slotd)
    let size = ^slot-value(o, ^size-slot-descriptor(slotd));
    for (i from 0 below size)
      f(slotd, i, ^repeated-slot-value(o, slotd, i));
    end;
  end;
end method;

define macro for-layout-fixed-slot-value
  { for-layout-fixed-slot-value (?:variable in ?:expression) ?:body end }
    => { do-layout-fixed-slot-values
           (method (_dummy, ?variable) ?body end, ?expression) }
  { for-layout-fixed-slot-value
        (?:variable described-by ?desc:variable in ?:expression)
      ?:body
    end }
    => { do-layout-fixed-slot-values
           (method (?desc, ?variable) ?body end, ?expression) }
end macro;

define macro for-layout-repeated-slot-value
  { for-layout-repeated-slot-value (?:variable in ?:expression) ?:body end }
    => { do-layout-repeated-slot-values
           (method (_dummy, _dummy-index, ?variable) ?body end, ?expression) }
  { for-layout-repeated-slot-value
        (?:variable described-by ?desc:variable keyed-by ?index:name
           in ?:expression)
      ?:body
    end }
    => { do-layout-repeated-slot-values
           (method (?desc, ?index, ?variable) ?body end, ?expression) }
end macro;
