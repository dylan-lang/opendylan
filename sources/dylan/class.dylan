Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <class> ... end;

define constant class-instance-header
  = method (class :: <class>) => (header :: <mm-wrapper>)
      class.class-mm-wrapper
    end method;

define inline function iclass-instance-header (iclass :: <implementation-class>)
 => (header :: <mm-wrapper>)
  iclass.class-mm-wrapper
end function;

define constant $max-class-log-size = 16; // TIED TO COMPILER

define leaf packed-slots class-properties (<implementation-class>, <object>)
  field    slot instance-storage-size = 0,  field-size:   $max-class-log-size;

  boolean  slot class-abstract?       = #f, init-keyword: abstract?:;
  boolean  slot class-primary?        = #f, init-keyword: primary?:;
  boolean  slot class-sealed?         = #f, init-keyword: sealed?:;
  boolean  slot iclass-type-complete? = #f;

  boolean  slot class-complete?       = #f;
  boolean  slot class-incremental?    = #f, init-keyword: incremental?:;
  boolean  slot slots-have-fixed-offsets?-bit       = #f,
    init-keyword: slots-have-fixed-offsets?:;
  boolean  slot slots-have-fixed-offsets?-computed? = #f,
    init-keyword: slots-have-fixed-offsets?-computed?:;

  boolean slot iclass-instantiable? = #f;

  boolean slot iclass-subclasses-fixed? = #f;
end packed-slots;

ignore(slots-have-fixed-offsets?-bit);
ignore(slots-have-fixed-offsets?-computed?);

// BOOTED: define ... class <function-class> ... end;
// BOOTED: define ... class <value-class> ... end;

define open generic as (type :: <type>, object) => object;

define open generic make 
    (type :: <type>, #rest key-value-pairs, #key, #all-keys) => object;

define open generic initialize 
    (instance, #key, #all-keys);

define generic subclass?
    (class-1 :: <class>, class-2 :: <class>) => (result :: <boolean>);


//// Subjunctive Class Attributes


define primary class <scu-node> (<object>)
  constant slot scu-node-next :: <scu-node>, init-keyword: next:;
  constant slot scu-node-class :: <class>, init-keyword: class:;
  constant slot scu-node-iclass :: <implementation-class>, init-keyword: implementation-class:;
  slot scu-node-initialized? :: <boolean> = #f;
end class;

define variable $empty-scu-node = make(<scu-node>);


define primary class <subjunctive-class-universe> (<object>)
  slot scu-alist :: <scu-node> = $empty-scu-node;
  slot scu-converter :: <function>;
end class;

define method initialize (scu :: <subjunctive-class-universe>, #key)
  scu-converter(scu) := method (c :: <class>) => (ic :: <implementation-class>)
			  scu-entry(c, scu)
			end method;
end method;

define sealed domain make (subclass(<subjunctive-class-universe>));
define sealed domain initialize (<subjunctive-class-universe>);

define function make-empty-subjunctive-class-universe ()
  make(<subjunctive-class-universe>)
end function;


define function scu-entry? (c :: <class>, scu :: <subjunctive-class-universe>)
 => (ans :: false-or(<implementation-class>))
  iterate loop (n :: <scu-node> = scu-alist(scu))
    if (n == $empty-scu-node)
      #f
    elseif (scu-node-class(n) == c)
      scu-node-iclass(n)
    else
      loop(scu-node-next(n))
    end if
  end iterate
end function;
      

define inline function scu-entry (c :: <class>, scu :: <subjunctive-class-universe>)
 => (ans :: <implementation-class>)
  (scu ~== $empty-subjunctive-class-universe & scu-entry?(c, scu))
    | 
  class-implementation-class(c)
end function;


define function scu-initialize-all (f :: <function>, scu :: <subjunctive-class-universe>)
 => ()
  // Optimize this stuff..
  scu-do(method (ic :: <implementation-class>, scu :: <subjunctive-class-universe>)
	   scu-force-initialization(iclass-class(ic), scu, f)
	 end,
	 scu);
end function;


define function scu-force-initialization (c :: <class>, 
					  scu :: <subjunctive-class-universe>,
					  f :: <function>)
 => (iclass :: <implementation-class>);
  iterate loop (n :: <scu-node> = scu-alist(scu))
    if (n == $empty-scu-node)
      class-implementation-class(c)
    elseif (scu-node-class(n) == c)
      let iclass :: <implementation-class> = scu-node-iclass(n);
      if (~(scu-node-initialized?(n)))
	for (c :: <class> in direct-superclasses(iclass)) scu-force-initialization(c, scu, f) end;
	if (~(scu-node-initialized?(n)))
	  f(iclass, scu);
	  scu-node-initialized?(n) := #t;
	end if
      end if;
      iclass
    else
      loop(scu-node-next(n))
    end if
  end iterate
end function;


define function scu-entry-setter (ic :: <implementation-class>, 
				  c :: <class>,
				  scu :: <subjunctive-class-universe>)
 => (ic :: <implementation-class>)
  if (scu == $empty-subjunctive-class-universe)
    error("Attempt to modify the canonical empty subjunctive class universe!")
  end if;
  iterate loop (n :: <scu-node> = scu-alist(scu))
    if (n == $empty-scu-node)
      scu-alist(scu) := make(<scu-node>, class: c, implementation-class: ic, next: scu-alist(scu))
    else
      loop(scu-node-next(n))
    end if
  end iterate;
  ic
end function;


define function scu-do (fn :: <function>, scu :: <subjunctive-class-universe>) => ()
  iterate loop (n :: <scu-node> = scu-alist(scu))
    if (n == $empty-scu-node)
      #f
    else
      fn(scu-node-iclass(n), scu);
      loop(scu-node-next(n))
    end if
  end iterate
end function;


define function all-iclass-superclasses (x :: <implementation-class>, u :: <subjunctive-class-universe>)
  map(scu-converter(u), all-superclasses(x))
end function;



/////
///// CREATION AND INITIALIZATION OF INSTANCES
/////

define function effective-initialization-argument-descriptor
    (descriptor :: <slot-descriptor>, iclass :: <implementation-class>) =>
    (descriptor :: <slot-keyword-initialization-descriptor>)
  let owner = slot-owner(descriptor);
  if (owner == iclass-class(iclass))
    descriptor				// optimize common case
  else
    let keyword = init-keyword(descriptor);
    if (keyword)
      block (return)
	for-each-superclass (c :: <class> of iclass)
          if (c == owner) return(descriptor) end;
	  for (d :: <init-arg-descriptor> in
		 direct-initialization-argument-descriptors(c))
	    if (init-keyword(d) == keyword &
		  (init-keyword-required?(d) | init-supplied?(d)))
	      return(d)
	    end;
	  end;
	end;
	descriptor
      end
    else
      descriptor
    end
  end
end;

define function effective-initial-value-descriptor
    (descriptor :: <slot-descriptor>, iclass :: <implementation-class>) =>
    (descriptor :: <slot-initial-value-descriptor>)
  let owner = slot-owner(descriptor);
  if (owner == iclass-class(iclass))
    descriptor				// optimize common case
  else
    block (return)
      let getter = slot-getter(descriptor);
      for-each-superclass (c :: <class> of iclass)
        if (c == owner) return(descriptor) end;
	for (d :: <inherited-slot-descriptor> in
	       direct-inherited-slot-descriptors(c))
	  if (inherited-slot-getter(d) == getter & init-supplied?(d))
	    return(d)
	  end;
	end;
      end;
      descriptor
    end
  end
end;

define class <slot-type-error> (<type-error>)
  constant slot slot-type-error-slot-descriptor :: <slot-descriptor>,
    required-init-keyword: slot-descriptor:;
end;

define method make
    (class == <slot-type-error>, #rest keys, #key value, type, slot-descriptor)
 => (error :: <slot-type-error>)
  apply(next-method, class,
        format-string: "Incorrect type for the %= init-keyword to %=.  "
          "The given value, %=, is not of type %=.",
        format-arguments: list(slot-descriptor.init-keyword,
                               slot-descriptor.slot-owner,
                               value,
                               slot-descriptor.slot-type),
	keys)
end method make;

define function keyword-value
    (descriptor :: <slot-descriptor>, iclass :: <implementation-class>,
     init-args :: <simple-object-vector>)
  local method keyword-element
            (v :: <simple-object-vector>, keyword :: <symbol>)
	  let v-size = size(v);
	  for (offset :: <integer> from 0 below v-size by 2,
	       until: vector-element(v, offset) == keyword)
	  finally if (offset < v-size)
		    vector-element(v, offset + 1)
		  else
		    $not-found
		  end if
	  end for;
	end method;
  let keyword = descriptor.init-keyword;
  if (keyword)
    let keyword :: <symbol> = keyword;// *@@* TYPE ONLY
    let keyword-value = keyword-element(init-args, keyword);
    if (keyword-value == $not-found)
      if (init-keyword-required?
	    (effective-initialization-argument-descriptor(descriptor, iclass)))
        error(make(<missing-keyword-error>,
                   format-string: "Make %= required-init-keyword %= not supplied.",
                   format-arguments: list(iclass-class(iclass), descriptor.init-keyword)))
      end if;
    elseif (~instance?(keyword-value, slot-type(descriptor)))
      error(make(<slot-type-error>,
                 value: keyword-value,
		 type: slot-type(descriptor),
		 slot-descriptor: descriptor))
    end if;
    keyword-value
  else
    $not-found
  end if
end function keyword-value;


// We need to be careful here.  If we just initialize $slot-initial-data-lock
// to a simple lock then the associated call to make may end up calling 
// init-data before the lock has been initialized.  Hence the two-stage process
// and the conditional code in init-data.  Is there a better way?

define variable $slot-initial-data-lock = #f;
$slot-initial-data-lock := make(<recursive-lock>);

define inline-only function init-data
    (descriptor :: <slot-initial-value-descriptor>) => (data)
  unless (descriptor.init-evaluated?)
    if (*dylan-library-initialized?*)
      with-lock ($slot-initial-data-lock)
        unless (descriptor.init-evaluated?)
          descriptor.init-data-slot := descriptor.init-data-slot();
          synchronize-side-effects();
          descriptor.init-evaluated? := #t;
        end unless
      end
    else
      descriptor.init-data-slot := descriptor.init-data-slot();
      descriptor.init-evaluated? := #t;
    end if
  end unless;

  descriptor.init-data-slot
end;


// This is intended for patchups only, to get around problems with model circularities
// in some critical classes.  It doesn't worry about thread safety because it is expected
// to be called before any threads exist, during dylan library initialization.
define function kludge-up-init-value (class :: <class>, getter :: <generic-function>, value)
  block (return)
    for (sd :: <slot-descriptor> in slot-descriptors(class))
      if (getter == slot-getter(sd))
	sd.init-data-slot := value;
	sd.init-evaluated? := #t;
	sd.init-value? := #t;
	return()
      end if
    end for;
    error("can't find %= in %=", getter, class);
  end
end function;
	

define inline-only function init-falue
    (descriptor :: <slot-initial-value-descriptor>)
 => (value)
  if (descriptor.init-supplied?)
    let data = descriptor.init-data;
    if (descriptor.init-value?)
      data
    else
      data()
    end
  else
    %unbound
  end
end;

// We arrange for this part to be done out of line.

define function install-and-return-make-method-init-data
    (descriptor :: <slot-initial-value-descriptor>) => (value)
  descriptor.init-data
end function;

define inline-only function make-method-init-value 
    (descriptor :: <slot-initial-value-descriptor>) => (value)
  if (descriptor.init-evaluated?)
    descriptor.init-data-slot
  else
    install-and-return-make-method-init-data(descriptor)
  end
end function;

define inline-only function make-method-init-function-value 
    (descriptor :: <slot-initial-value-descriptor>) => (value)
  if (descriptor.init-evaluated?)
    descriptor.init-data-slot()
  else
    (install-and-return-make-method-init-data(descriptor))();
  end
end function;

define inline-only function instance-new-value
    (descriptor :: <slot-descriptor>, iclass :: <implementation-class>,
     init-args :: <simple-object-vector>) => (value)
  let keyword-value = keyword-value(descriptor, iclass, init-args);
  if (keyword-value == $not-found)
    init-falue(effective-initial-value-descriptor(descriptor, iclass))
  else
    keyword-value
  end if
end;

define method allocation-attributes
    (iclass :: <implementation-class>, init-args :: <simple-object-vector>)
 => (instance-size :: <integer>, 
     repeated-slot? :: <boolean>, repeated-slot-type :: <type>,
     repeated-size :: <integer>, fill-value)
  let instance-size = iclass.instance-storage-size;
  let repeated-slot-descriptor = iclass.repeated-slot-descriptor;
  if (repeated-slot-descriptor)
    let repeated-slot-descriptor :: <repeated-slot-descriptor>
      = repeated-slot-descriptor; // *@@* TYPE
    values(instance-size, #t, repeated-slot-descriptor.slot-type,
           instance-new-value
	     (repeated-slot-descriptor.size-slot-descriptor, iclass, init-args),
           instance-new-value(repeated-slot-descriptor, iclass, init-args))
  else
    values(instance-size, #f, <object>, 0, #f)
  end if 
end method allocation-attributes;


define function allocate-instance
    (class :: <class>, init-args :: <simple-object-vector>) => (instance)
  allocate-instance-i(class-implementation-class(class), init-args)
end function;


define function class-not-instantiable (ic :: <implementation-class>) => ()
  let c :: <class> = ic.iclass-class;
  if (class-abstract?(ic))
    error("Cannot instantiate an abstract class - %=", c)
  elseif (~class-complete?(ic))
    unless (#f /* attempt-deferred-finalization(ic) */)
      error("Cannot instantate %= - its superclass expressions have not finished executing", c)
    end unless
  else
    // Or, we're broken..
    error("Cannot instantiate %=, it is not an instantiable type.", c)
  end if
end function;

    
define function allocate-instance-i
    (iclass :: <implementation-class>, init-args :: <simple-object-vector>) => (instance)
  let class :: <class> = iclass-class(iclass);
  if (~iclass-instantiable?(iclass))
    // This either errors and doesn't return, or returns having successfully
    // performed deferred completeness of the (implementation)class.
    class-not-instantiable(iclass)
  end if;
  for (i :: <integer> from 0 below size(class-slot-descriptors(iclass)))
    find-or-create-class-slot-storage(iclass, i, #t)
  end for;
  let (instance-size :: <integer>,
       repeated-slot? :: <boolean>, repeated-slot-type :: <type>,
       repeated-size :: <integer>, fill)
    = allocation-attributes(iclass, init-args);

  if (repeated-slot?)
    system-allocate-repeated-instance
      (class, repeated-slot-type, unbound(), repeated-size, fill)
  else
    system-allocate-simple-instance-i(iclass)
  end if;
end function;

define class <class-incomplete-error> (<simple-condition>, <incomplete-error>)
end;

define constant class-incomplete-error-class :: <function> = incomplete-object;


// don't discriminate on class, it is broken anyway
define function report-class-incomplete (class, format-string :: <string>)
 => (will-never-return :: <bottom>)
  let  class-rep = class.debug-name ; // incomplete class is a dangerous out-of-language thing
  error (make (<class-incomplete-error>,
	 object:           class-rep,
	 format-string:    format-string, 
	 format-arguments: list (class-rep)))
end;

define function default-class-constructor
    (class :: <class>, #rest init-args, #key, #all-keys)
  unless (class.class-complete?)
    report-class-incomplete (class, "attempt to make an instance of an incompletely initialized class: %=")
  end;

  let init-args 
    = concatenate-2(init-args, class.defaulted-initialization-arguments);
  let instance = allocate-instance(class, init-args);

  apply(default-initialize, class, instance, init-args);
  apply(initialize, instance, init-args);

  instance
end function default-class-constructor;

define function default-initialize
    (class :: <class>, instance, #rest init-args, #key, #all-keys)

  let iclass = instance.object-implementation-class;

  // INSTANCE / CONSTANT

  let descriptors :: <simple-object-vector>
    = iclass.instance-slot-descriptors;
  for (slot-offset :: <integer> from 0 below descriptors.size)
    let descriptor :: <slot-descriptor> 
      = vector-element(descriptors, slot-offset);
    primitive-slot-value(instance, integer-as-raw(slot-offset))
      := instance-new-value(descriptor, iclass, init-args);
  end for;

  // CLASS / EACH-SUBCLASS

  let class-descriptors :: <simple-object-vector>
    = class-slot-descriptors(iclass);
  for (offset :: <integer> from 0 below size(class-descriptors))
    let descriptor :: <any-class-slot-descriptor>
      = vector-element(class-descriptors, offset);
    let init = if (descriptor.init-keyword)
                 keyword-value(descriptor, iclass, init-args)
               else
                 $not-found
               end if;
    // This ensures that the storage has been created.  (Should really be
    // done by some finalize action...)  We let find-or-create... do the
    // init value compuation if we don't have a keyword initializer here.
    let cell :: <pair>
      = find-or-create-class-slot-storage(iclass, offset, init == $not-found);
    if (init ~== $not-found)
      // TODO:  check type ...
      head(cell) := init
    end if
  end for;

  instance
end function default-initialize;

define function defaulted-initialization-arguments 
    (class :: <class>) => (defaulted :: <simple-object-vector>)
  let slot = defaulted-initialization-arguments-slot(class);
  if (instance?(slot, <simple-object-vector>))
    slot
  else
    let slot :: <integer> = slot;
    let result :: <simple-object-vector> = make(<simple-object-vector>, size: abs(slot));
    let index :: <integer> = 0;
    let required-keywords = #();
    for-each-superclass (c :: <class> of class)
      for (descriptor :: <init-arg-descriptor> in 
	     direct-initialization-argument-descriptors(c))
	let keyword = descriptor.init-keyword;
	if (member?(keyword, required-keywords))
	  // don't look at it
	elseif (descriptor.init-keyword-required?)
	  required-keywords := pair(keyword, required-keywords);
	elseif (descriptor.init-supplied? &
		  ~(for (i :: <integer> from 0 below index by 2,
			 until: keyword == result[i])
		      // if we quit early, then we found the keyword
		    finally let idx :: <integer> = index; i < idx
		    end))
	  let idx :: <integer> = index;
	  result[idx] := keyword;
	  result[idx + 1] := init-falue(descriptor);
	  index := idx + 2;
	end;
      end;
    end;
    if (negative?(slot))
      // they're all init-values, so remember them
      defaulted-initialization-arguments-slot(class) := result;
    end;
    result
  end
end;

define method reinitialize (instance, #rest init-args)
  let class = object-class(instance);
  let init-args 
    = concatenate-2(init-args, class.defaulted-initialization-arguments);
  apply(default-initialize, class, instance, init-args);
  apply(initialize, instance, init-args);
end method reinitialize;

// This is a hook for the compiler that allows it to optimize the
// step class -> constructor without leaving a bare implementation 
// class object in the code at any time.

define function class-constructor-atomically
    (class :: <class>) => (constructor :: <method>)
  class-constructor(class)
end function;

define inline method make
    (class :: <class>, #rest init-args, #key, #all-keys) => (object)
  apply(class-constructor-atomically(class), class, init-args);
end method make;

///
/// RCPL MAINTENANCE
///


// Appropriate locking is presumed.
define function initialize-class-instance?-iep (c :: <class>) => ();
  let m :: <simple-method>
    = if (class-subtype-bit(c) ~== 0)
	masked-class-instance?
      else
	let pos :: <integer> = class-rcpl-position(c);
	let v :: <simple-object-vector> = class-rcpl-other-positions(c);
	let nothers :: <integer> = size(v);
	if (nothers > 0)
	  general-rcpl-class-instance?
	elseif (pos < 0)
	  class-instance?-initial
	elseif (pos < $min-rcpl-size)
	  class-instance?-rcpl-single-small
	else
	  class-instance?-rcpl-single-large
	end if
      end if;
  c.instance?-iep := m.simple-method-iep;
end function;

define function invalidate-class-instance?-iep (c :: <class>) => ();
  c.instance?-iep := never-instance?-function.simple-method-iep;
end function;

define constant never-instance?-function = method (x, t)
  #f
end method;


define constant $class-bashing-lock :: <simple-lock>
  = make-simple-lock();

define inline function rcpl-position-known? (pos :: <integer>, cls :: <class>) => v :: <boolean>;
  pos == cls.class-rcpl-position | member?(pos, cls.class-rcpl-other-positions)
end function;

define inline function iclass-rcpl-position-known? (pos :: <integer>, iclass :: <implementation-class>) => v :: <boolean>;
  pos == iclass.class-rcpl-position | member?(pos, iclass.class-rcpl-other-positions)
end function;


// incrementally extend position vectors as required, keep sorted.
define function augment-iclass-rcpl-position-data (iclass :: <implementation-class>, pos :: <integer>) => ();
  let lk :: <simple-lock> = $class-bashing-lock;
  iclass-rcpl-position-known?(pos, iclass)
    | with-lock(lk)
        iclass-rcpl-position-known?(pos, iclass)
          | begin
	      let first-pos :: <integer> = iclass.class-rcpl-position;
	      if (first-pos == -1)
		iclass.class-rcpl-position := pos;
	      else
		assert(pos >= first-pos);
		let add-me :: <integer> = pos;
		let others :: <simple-object-vector> = iclass.class-rcpl-other-positions;
		let nothers :: <integer> = size(others);
		let new :: <simple-object-vector> = make(<simple-object-vector>, size: nothers + 1);
		local method foo (i :: <integer>)
			if (i == nothers)
			  new[i] := add-me;
			else
			  let elt :: <integer> = others[i];
			  if (elt < add-me)
			    new[i] := elt;
			    foo(i + 1)
			  else
			    new[i] := add-me;
			    local method bar (i :: <integer>, j :: <integer>)
				    if (i ~== nothers)
				      new[j] := others[i];
				      bar(j, j + 1)
				    end if
				  end method;
			    bar(i, i + 1)
			  end if
			end if
		      end method;
		foo(0);
		synchronize-side-effects();
		iclass.class-rcpl-other-positions := new;
		// It is implicit that, in simple additions to the positions, the instance? code
		// is upwards-compatible (although the newly added type might not be recognized
		// until the new instance? function is added).  If this gets changed we will need
		// a more complicated mechanism.
	      end if;
	      synchronize-side-effects();
	      let the-class :: <class> = iclass-class(iclass);
	      if (class-implementation-class(the-class) == iclass)
		initialize-class-instance?-iep(the-class)
	      end if;
	    end
    end with-lock
end function;


define function augment-rcpl-position-data-multiple (cls :: <class>, positions :: <simple-object-vector>) 
 => ()
  let lk :: <simple-lock> = $class-bashing-lock;
  with-lock (lk)
    let ninc :: <integer> = 0;
  let npos :: <integer> = size(positions);
  let ninc :: <integer> = begin
			    local method loop (i :: <integer>, n :: <integer>)
				    if (i == 0)
				      n
				    else
				      let i :: <integer> = i - 1;
				      let p :: <integer> = vector-element(positions, i);
				      loop(i, if (rcpl-position-known?(p, cls)) n else n + 1 end)
				    end if
				  end method;
			    loop(size(positions), 0)
			  end;
  // for (p :: <integer> in positions) if (~rcpl-position-known?(p, cls)) ninc := ninc + 1 end if end for;
  if (ninc > 0)
    let old :: <simple-object-vector> = cls.class-rcpl-other-positions;
    let nold :: <integer> = size(old);
    let nnew :: <integer> = nold + ninc;
    let new :: <simple-object-vector> = make(<simple-object-vector>, size: nnew);
    let base-pos :: <integer> = class-rcpl-position(cls);
    local method loop (pidx :: <integer>, nidx :: <integer>, oidx :: <integer>)
	    if (oidx == nold)
	      if (pidx ~== npos)
		new[nidx] := positions[pidx];
		loop(pidx + 1, nidx + 1, oidx)
	      else
		synchronize-side-effects();
		cls.class-rcpl-other-positions := new;
		synchronize-side-effects();
		initialize-class-instance?-iep(cls)
	      end if
	    elseif (pidx == npos)
	      new[nidx] := old[oidx];
	      loop(pidx, nidx + 1, oidx + 1)
	    else
	      let olde :: <integer> = old[oidx];
	      let newe :: <integer> = positions[pidx];
	      if (newe < olde)
		if (newe == base-pos)
		  loop(pidx + 1, nidx, oidx)
		else
		  new[nidx] := newe;
		  loop(pidx + 1, nidx + 1, oidx)
		end if
	      else
		new[nidx] := olde;
		loop(if (olde == newe) pidx + 1 else pidx end, nidx + 1, oidx + 1)
	      end if
	    end if
	  end method;
    loop(0, 0, 0)
  end if
end with-lock
end function;


define function augment-rcpl-position-data-kludgey (data :: <simple-object-vector>) => ()
  let n :: <integer> = size(data);
  local method loop(i :: <integer>)
	  if (i < n)
	    let c :: <class> = data[i];
	    let p :: <integer> = data[i + 1];
	    augment-iclass-rcpl-position-data(class-implementation-class(c), p);
	    loop(i + 2)
	  end if
	end method;
  loop(0)
end function;


////
//// FAST INSTANCE? SUPPORT
////


define inline constant $min-rcpl-size = 6;


define inline function rcpl-isubclass? (isub :: <implementation-class>, sub :: <class>,
					isup :: <implementation-class>, sup :: <class>)
  if (pointer-id? (sub, sup))
    #t
  else
    let  position  = isup.class-rcpl-position ;
    let  rcpl      = isub.class-rcpl-vector ;
    let  rcpllen   = rcpl.size;
    if (position >= rcpllen)
      #f
    elseif (pointer-id? (vector-element(rcpl, position), sup))
      #t
    else
      let  positions = isup.class-rcpl-other-positions ;
      let  len :: <integer> = positions.size ;
      local method loop (i :: <integer>)
	      if (i == len)
		#f
	      else
		let rpos :: <integer> = vector-element(positions, i);
		if (rpos < rcpllen)
		  if (pointer-id?(vector-element(rcpl, rpos), sup))
		    #t
		  else
		    loop(i + 1)
		  end if
		else
		  #f
		end if
	      end if
	    end method;
      loop(0)
    end if
  end if
end;

 
define constant general-rcpl-class-instance? = method
    (obj :: <object>, cls :: <class>) => (v :: <boolean>)
  let icls :: <implementation-class> = cls.class-implementation-class;
  // let n :: <integer> = class-instance?-count(icls) + 1;
  // class-instance?-count(icls) := n;
  let objicls :: <implementation-class> = obj.object-implementation-class;
  rcpl-isubclass?(objicls, objicls.iclass-class, icls, cls)
end method;

define constant class-instance?-initial = method
    (obj :: <object>, cls :: <class>) => (v :: <boolean>)
  // let icls :: <implementation-class> = cls.class-implementation-class;
  // let n :: <integer> = class-instance?-count(icls) + 1;
  // class-instance?-count(icls) := n;
  indirect-object?(obj) 
    &
    pointer-id?(iclass-class(indirect-object-implementation-class(obj)), cls)
end method;

define constant class-instance?-rcpl-single-large = method
    (obj :: <object>, cls :: <class>) => (v :: <boolean>)
  let icls :: <implementation-class> = cls.class-implementation-class;
  // let n :: <integer> = class-instance?-count(icls) + 1;
  // class-instance?-count(icls) := n;
  let isubc :: <implementation-class> = obj.object-implementation-class;
  let subc :: <class> = isubc.iclass-class;
  if (pointer-id? (cls, subc))
    #t
  else
    let  position  = icls.class-rcpl-position ;
    let  rcpl      = isubc.class-rcpl-vector ;
    let  rcpllen   = rcpl.size ;
    (position < rcpllen)
      &
      pointer-id?(vector-element(rcpl, position), cls)
  end if
end method;

define constant class-instance?-rcpl-single-small = method
    (obj :: <object>, cls :: <class>) => (v :: <boolean>)
  let icls :: <implementation-class> = cls.class-implementation-class;
  // let n :: <integer> = class-instance?-count(icls) + 1;
  // class-instance?-count(icls) := n;
  let isubc :: <implementation-class> = obj.object-implementation-class;
  let subc :: <class> = isubc.iclass-class;
  if (pointer-id? (subc, cls))
    #t
  else
    let  position  = icls.class-rcpl-position ;
    let  rcpl      = isubc.class-rcpl-vector ;
    pointer-id?(vector-element(rcpl, position), cls)
  end if
end method;

// Used by compiler.
define constant <object>-class-instance? = method
    (obj :: <object>, cls == <object>) => (v == #t)
  #t
end method;


define constant masked-class-instance? = method (x, c :: <class>) => (v :: <boolean>);
  // let cnt :: <integer> = class-instance?-count(icls) + 1;
  // class-instance?-count(icls) := cnt;
  logand(mm-wrapper-subtype-mask(object-mm-wrapper(x)), class-subtype-bit(c)) ~== 0
end method;


define constant get-class-instance?-counts 
  = method (classes :: <sequence>) => (v :: <sequence>);
      map(class-instance?-count, classes)
    end method;

//// Class instance counting.

// Comment out these stubs and reinstate the slot in class objects in order
// to re-enable instance check counting.

define inline-only method class-instance?-count 
    (c :: <class>) => (count :: <integer>)
  0
end method;

define inline-only method class-instance?-count-setter
    (count :: <integer>, c :: <class>) => (count :: <integer>)
  0
end method;

  
////
//// BASIC OPERATIONS
////

define inline method subclass? (x :: <class>, y :: <class>) => (result :: <boolean>)
  // The following pre-check is not really necessary, and is not correct if the
  // runtime is capable of representing raw types.  I had been planning on doing
  // an optimization of rcpl management where <object> would not be stored in an
  // rcpl vector, but has not yet been done, and is similarly incompatible.  Also
  // that optimization makes it harder to eliminate the cpl in favor of the rcpl vector.
//  if (y == <object>)
//    #t  // special grounding case, we can assume x is a subclass of <object>, even if broken.
//  elseif ...
  subiclass?(x.class-implementation-class, x, y.class-implementation-class, y)
end method;

define function subiclass? (xiclass :: <implementation-class>, x :: <class>,
			    yiclass :: <implementation-class>, y :: <class>)
 => (well? :: <boolean>)
  if (xiclass.iclass-type-complete?)
    if (yiclass.iclass-type-complete?)
      rcpl-isubclass?(xiclass, x, yiclass, y)
    else
      report-class-incomplete (y, "attempt to call subclass? on an incompletely initialized class %=")
    end
  else
    report-class-incomplete (x, "attempt to call subclass? on an incompletely initialized class %=")
  end
end function;


define method subtype? (class-1 :: <class>, class-2 :: <class>)
 => (result :: <boolean>)
  subclass?(class-1, class-2)
end method subtype?;

define inline method subjunctive-subtype? (class-1 :: <class>, class-2 :: <class>,
					   scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  subiclass?(scu-entry(class-1, scu), class-1, scu-entry(class-2, scu), class-2)
end method;


// !@#$ SHOULD BE SHARED WITH DISPATCH CODE

define method has-instances? (class-1 :: <class>, class-2 :: <class>,
			      scu :: <subjunctive-class-universe>)
  => (some? :: <boolean>, all? :: <boolean>);
  let ic1 :: <implementation-class> = scu-entry(class-1, scu);
  let ic2 :: <implementation-class> = scu-entry(class-2, scu);
  if (subiclass?(ic1, class-1, ic2, class-2))
    values(#t, #t)
  else
    values(#f, #f)
  end if;
end method has-instances?;


define method augment-class-known-joint (c1 :: <class>, vec :: <simple-object-vector>)
  (with-lock ($class-bashing-lock)
    let new :: <list> = #();
    let ic1 :: <implementation-class> = class-implementation-class(c1);
    let known :: <simple-object-vector> = class-known-joint(ic1);
    for (c2 :: <class> in vec)
      let ic2 :: <implementation-class> = class-implementation-class(c2);
      if (~member?(c1, class-known-joint(ic2)) & ~member?(c2, known))
	new := pair(c2, new)
      end if
    end for;
    if (~empty?(new))
      class-known-joint(ic1) := concatenate(known, new)
    end if
  end with-lock
  )
end method;


define function grounded-member?
    (value, collection :: <simple-object-vector>)
	=> (boolean :: <boolean>)
  without-bounds-checks
    let n :: <integer> = collection.size;
    iterate grovel (index :: <integer> = 0)
      if (index = n)
	#f
      elseif (pointer-id?(collection[index], value))
	#t 
      else
	grovel(index + 1)
      end
    end iterate
  end without-bounds-checks;
end function;

define method disjoint-types-1? (t1 :: <class>, t2 :: <class>,
				 scu :: <subjunctive-class-universe>,
				 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  let ic1 :: <implementation-class> = scu-entry(t1, scu);
  let ic2 :: <implementation-class> = scu-entry(t2, scu);
  let dis?
    = (~subiclass?(ic1, t1, ic2, t2)
	 & ~subiclass?(ic2, t2, ic1, t1)
	 & ( (class-primary?(ic1) & class-primary?(ic2))
	      |
	      (~grounded-member?(t1, ic2.class-known-joint)
		 & ~grounded-member?(t2, ic1.class-known-joint))));
  when (dis? & dep)
    %register-disjoint-dependent-generic(t1, t2, dep);
  end when;
  dis?
end method;



define inline method type-complete? (t :: <class>) => (well? :: <boolean>)
  iclass-type-complete?(class-implementation-class(t))
end method;


define inline method map-congruency-classes (f :: <function>, t :: <class>) => ()
  f(t)
end method;


define method reduce-incomplete-classes (f :: <function>, t :: <class>, ans)
 => (ans)
  if (~iclass-type-complete?(class-implementation-class(t)))
    f(t, ans)
  else
    ans
  end if
end method;

define variable *class-profiling-enabled?* :: <boolean> = #f;


// eof
 
