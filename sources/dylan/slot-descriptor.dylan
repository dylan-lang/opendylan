Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <slot-descriptor> ... end;

define packed-slots slot-descriptor-properties
    (<slot-initial-value-descriptor>, <object>)
  boolean slot init-supplied?  = #f, init-keyword: init-supplied?:;
  boolean slot init-value?     = #f, init-keyword: init-value?:;
  boolean slot init-evaluated? = #f, init-keyword: init-evaluated?:;
end packed-slots;

define packed-slots slot-descriptor-properties
    (<slot-keyword-initialization-descriptor>, <slot-initial-value-descriptor>)
  boolean slot init-keyword-required? = #f,
    init-keyword: init-keyword-required?:;
end packed-slots;

define leaf packed-slots slot-descriptor-properties
    (<slot-descriptor>, <slot-keyword-initialization-descriptor>)
  field slot slot-storage-size = 1, field-size: 8,
    init-keyword: storage-size:;
  boolean  slot slot-method-sealed?    = #f, init-keyword: slot-method-sealed?:;
end packed-slots;


define generic slot-allocation
  (slot-descriptor :: <slot-descriptor>) => (result :: <symbol>);

define generic slot-descriptor
  (instance, getter :: <function>) => (result :: <slot-descriptor>);

define open generic slot-initialized?
    (instance, slot-descriptor :: type-union(<slot-descriptor>, <function>))
 => (boolean :: <boolean>);

define generic slot-value
  (instance, slot-descriptor :: <slot-descriptor>) => value;

define generic slot-value-setter
  (new-value, instance, slot-descriptor) => new-value;

// TODO: OBSOLETE?

/*
define generic repeated-slot-initialized?
    (instance, slot-descriptor :: <slot-descriptor>, offset :: <integer>)
 => boolean;
*/

define generic repeated-slot-value
    (instance, slot-descriptor :: <slot-descriptor>, offset :: <integer>)
 => value;

define generic repeated-slot-value-setter
    (new-value,
     instance, slot-descriptor :: <slot-descriptor>, offset :: <integer>)
 => new-value;

/// PRIVATE


define generic as-slot-descriptor-class (allocation);

//// RUN-TIME

define method getter=
    (descriptor-1 :: <slot-descriptor>, descriptor-2 :: <slot-descriptor>)
  descriptor-1.slot-getter == descriptor-2.slot-getter
end method getter=;

define method slot-descriptor (instance, getter :: <function>)
 => (result :: <slot-descriptor>)
  block (return)
    for (a-slot-descriptor in instance.object-implementation-class.slot-descriptors)
      if (getter == a-slot-descriptor.slot-getter)
        return(a-slot-descriptor)
      end if;
    end for
  end block
end method slot-descriptor;

/////
///// ABSTRACT CLASSES
/////

////
//// <ANY-INSTANCE-SLOT-DESCRIPTOR>
////

// BOOTED: define ... class <any-instance-slot-descriptor> ... end;

//// RUN-TIME

define abstract class <slot-error> (<error>) end;

define class <simple-slot-error> (<slot-error>, <simple-error>) end;


define method slot-initialized?
    (object, slot-descriptor :: <any-instance-slot-descriptor>)
 => (boolean :: <boolean>)
  let offset = slot-offset-i(slot-descriptor, object.object-implementation-class);
  if (offset)
    ~initialized-slot-element(object, offset).unbound?
  else
    error(make(<simple-slot-error>,
               format-string: "The Slot-Descriptor %= is missing from class %= of object %=",
               format-arguments: list(slot-descriptor, object.object-class, object)))
  end if
end method slot-initialized?;

define method slot-value
    (object, slot-descriptor :: <any-instance-slot-descriptor>)
 => (value)
  let offset = slot-offset-i(slot-descriptor, object.object-implementation-class);
  if (offset)
    let value = slot-element(object, offset);
    if (value.unbound?)
      error(make(<simple-slot-error>,
                 format-string: "The Slot-Descriptor %= is unbound in class %= of object %=",
                 format-arguments: list(slot-descriptor, object.object-class, object)))
    else
      value
    end if
  else
    error(make(<simple-slot-error>,
               format-string: "The Slot-Descriptor %= is missing from class %= of object %=",
               format-arguments: list(slot-descriptor, object.object-class, object)))
  end if
end method slot-value;

define method slot-value-setter
    (new-value, object, slot-descriptor :: <any-instance-slot-descriptor>)
 => (value)
  let offset = slot-offset-i(slot-descriptor, object.object-implementation-class);
  if (offset)
    if (instance?(new-value, slot-type(slot-descriptor)))
      slot-element(object, offset) := new-value;
    else
      error(make(<slot-type-error>, value: new-value,
                 type: slot-type(slot-descriptor),
                 slot-descriptor: slot-descriptor))
    end if
  else
    error(make(<simple-slot-error>,
               format-string: "slot-value-setter Slot-Descriptor %= is missing from class %= of object %=",
               format-arguments: list(slot-descriptor, object.object-class, object)))
  end if
end method slot-value-setter;

////
//// <ANY-CLASS-SLOT-DESCRIPTOR>
////

// Moved to boot.
// define class <any-class-slot-descriptor> (<slot-descriptor>)
// end class <any-class-slot-descriptor>;

//// RUN-TIME

define inline function iclass-slot-element (icls :: <implementation-class>, offset :: <integer>)
  => (value :: <object>)
  head(find-or-create-class-slot-storage(icls, offset, #t))
end function;

define inline function iclass-slot-element-setter
    (value, icls :: <implementation-class>, offset :: <integer>)
 => (value :: <object>)
  head(find-or-create-class-slot-storage(icls, offset, #f)) := value
end function;


define method slot-initialized?
    (object, slot-descriptor :: <any-class-slot-descriptor>)
 => (boolean :: <boolean>)
  let icls :: <implementation-class> = object-implementation-class(object);
  let offset = slot-offset-i(slot-descriptor, icls);
  if (offset)
    let offset :: <integer> = offset;  // TODO: TYPE ONLY
    ~iclass-slot-element(icls, offset).unbound?
  else
    error(make(<simple-slot-error>,
               format-string: "The Slot-Descriptor %= is missing from class %= of object %=",
               format-arguments: list(slot-descriptor, iclass-class(icls), object)))
  end if
end method slot-initialized?;


define method slot-value
    (object, slot-descriptor :: <any-class-slot-descriptor>)
 => (value)
  let icls :: <implementation-class> = object-implementation-class(object);
  let offset :: <integer> = slot-offset-i(slot-descriptor, icls);
  if (offset)
    let offset :: <integer> = offset; // HACK: TYPE ONLY
    let value = iclass-slot-element(icls, offset);
    if (value.unbound?)
      error(make(<simple-slot-error>,
                 format-string: "The Slot-Descriptor %= is unbound in class %=",
                 format-arguments: list(slot-descriptor, iclass-class(icls))))
    else
      value
    end if
  else
    error(make(<simple-slot-error>,
               format-string: "The Slot-Descriptor %= is missing from class %=",
               format-arguments: list(slot-descriptor, iclass-class(icls))))
  end if
end method slot-value;


define method slot-value-setter
    (new-value, object, slot-descriptor :: <any-class-slot-descriptor>)
 => (value)
  let icls :: <implementation-class> = object-implementation-class(object);
  let offset = slot-offset-i(slot-descriptor, icls);
  if (offset)
    if (instance?(new-value, slot-type(slot-descriptor)))
      let offset :: <integer> = offset; // HACK: TYPE ONLY
      iclass-slot-element(icls, offset) := new-value
    else
      error(make(<slot-type-error>, value: new-value,
                 type: slot-type(slot-descriptor),
                 slot-descriptor: slot-descriptor))
    end if
  else
    error(make(<simple-slot-error>,
               format-string: "slot-value-setter Slot-Descriptor %= is missing from class %=",
               format-arguments: list(slot-descriptor, iclass-class(icls))))
  end if
end method slot-value-setter;

/////
///// CONCRETE CLASSES
/////

////
//// <INSTANCE-SLOT-DESCRIPTOR>
////

// BOOTED: define ... class <instance-slot-descriptor> ... end;

//// CREATION

define method as-slot-descriptor-class (symbol == #"instance")
  <instance-slot-descriptor>
end method as-slot-descriptor-class;

////
//// <CONSTANT-SLOT-DESCRIPTOR>
////

define class <constant-slot-descriptor> (<any-instance-slot-descriptor>)
end class <constant-slot-descriptor>;

//// CREATION

define method slot-allocation (descriptor :: <constant-slot-descriptor>)
 => (result :: <symbol>)
  #"constant"
end method slot-allocation;

//// RUN-TIME

define method slot-initialized?
    (object, slot-descriptor :: <constant-slot-descriptor>)
 => (boolean :: <boolean>)
  #t
end method slot-initialized?;

define method slot-value-setter
    (new-value, object, slot-descriptor :: <constant-slot-descriptor>)
 => (value)
  error(make(<immutable-error>,
             format-string: "Invalid to set the value of a constant slot %=.",
             format-arguments: list(slot-descriptor)))
end method slot-value-setter;

////
//// <CLASS-SLOT-DESCRIPTOR>
////

// Moved to boot.
// define class <class-slot-descriptor> (<any-class-slot-descriptor>)
// end class <class-slot-descriptor>;

//// CREATION

define method slot-allocation (descriptor :: <class-slot-descriptor>)
 => (result :: <symbol>)
  #"class"
end method slot-allocation;


////
//// <EACH-SUBCLASS-SLOT-DESCRIPTOR>
////

// Moved to boot.
// define class <each-subclass-slot-descriptor> (<any-class-slot-descriptor>)
// end class <each-subclass-slot-descriptor>;

//// CREATION

define method slot-allocation (descriptor :: <each-subclass-slot-descriptor>)
 => (result :: <symbol>)
  #"each-subclass"
end method slot-allocation;



define function find-or-create-class-slot-storage
    (icls :: <implementation-class>, offset :: <integer>, initialize-now? :: <boolean>)
 => (cell :: <pair>)
  let storage :: <simple-object-vector> = class-slot-storage(icls);
  (vector-element(storage, offset)
    | begin
        let slots :: <simple-object-vector> = class-slot-descriptors(icls);
        let sd :: <any-class-slot-descriptor> = vector-element(slots, offset);
        let own :: <class> = slot-owner(sd);
        if (own == iclass-class(icls) | instance?(sd, <each-subclass-slot-descriptor>))
          let initv =
            if (initialize-now?)
              // init-falue(sd)
              init-falue(effective-initial-value-descriptor(sd, icls))
            else
              %unbound
            end;
          let cell :: <pair> = pair(initv, sd);
          vector-element(storage, offset) := cell
        else
          // Class allocation, inherited storage.  Get the owner's cell and save it.
          let get-from :: <implementation-class> = class-implementation-class(own);
          vector-element(storage, offset)
            := find-or-create-class-slot-storage(get-from, slot-offset-i(sd, get-from), initialize-now?)
        end if
      end)
end function;

////
//// <VIRTUAL-SLOT-DESCRIPTOR>
////

// BOOTED: define ... class <virtual-slot-descriptor> ... end;

//// CREATION

define method as-slot-descriptor-class (symbol == #"virtual")
  <virtual-slot-descriptor>
end method as-slot-descriptor-class;

//// RUN-TIME

define method slot-initialized?
    (object, slot-descriptor :: <virtual-slot-descriptor>)
 => (boolean :: <boolean>)
  // DEFAULT
  #t
end method slot-initialized?;

define method slot-value (object, slot-descriptor :: <virtual-slot-descriptor>)
 => (value)
  slot-descriptor.slot-getter(object)
end method slot-value;

define method slot-value-setter
    (new-value, object, slot-descriptor :: <virtual-slot-descriptor>)
 => (value)
  // !@#$ could optimize this
  if (slot-descriptor.slot-setter)
    slot-descriptor.slot-setter(new-value, object)
  end if
end method slot-value-setter;

////
//// <REPEATED-SLOT-DESCRIPTOR>
////

// BOOTED: define ... class <repeated-slot-descriptor> ... end;

define method repeated-byte-slot? (descriptor :: <repeated-slot-descriptor>)
  descriptor.slot-type == <byte-character>
end method repeated-byte-slot?;

//// CREATION

define method slot-allocation (descriptor :: <repeated-slot-descriptor>)
 => (result :: <symbol>)
  #"repeated"
end method slot-allocation;

//// RUN-TIME

// TODO: OBSOLETE?

/*
define method repeated-slot-initialized?
    (object,
     descriptor :: <repeated-slot-descriptor>,
     offset :: <integer>)
 => (value :: <boolean>)
  let base-offset = slot-offset-i(descriptor, object.object-implementation-class);
  if (base-offset)
    ~repeated-slot-element(object, base-offset, offset).unbound?
  else
    error(make(<simple-slot-error>,
               format-string: "The Slot-Descriptor %= is missing from class %= of object %=",
               format-arguments: list(descriptor, object.object-class, object)))
  end if
end method repeated-slot-initialized?;
*/

define method repeated-slot-value
    (object, descriptor :: <repeated-slot-descriptor>,
     offset :: <integer>)
 => (value)
  let base-offset = slot-offset-i(descriptor, object.object-implementation-class);
  if (base-offset)
    let value
      = if (descriptor.repeated-byte-slot?)
          byte-slot-element(object, base-offset, offset)
        else
          repeated-slot-element(object, base-offset, offset)
        end if;
    if (value.unbound?)
      error(make(<simple-slot-error>,
                 format-string: "The Slot-Descriptor %= is unbound in class %= of object %=",
                 format-arguments: list(descriptor, object.object-class, object)))
    else
      value
    end if
  else
    error(make(<simple-slot-error>,
               format-string: "The Slot-Descriptor %= is missing from class %= of object %=",
               format-arguments: list(descriptor, object.object-class, object)))
  end if
end method repeated-slot-value;

define method repeated-slot-value-setter
    (new-value,
     object, descriptor :: <repeated-slot-descriptor>,
     offset :: <integer>)
 => (value)
  let base-offset = slot-offset-i(descriptor, object.object-implementation-class);
  if (base-offset)
    if (descriptor.repeated-byte-slot?)
      byte-slot-element(object, base-offset, offset) := new-value
    else
      repeated-slot-element(object, base-offset, offset) := new-value
    end if
  else
    error(make(<simple-slot-error>,
               format-string: "slot-value-setter Slot-Descriptor %= is missing from class %= of object %=",
               format-arguments: list(descriptor, object.object-class, object)))
  end if
end method repeated-slot-value-setter;

////
////
////


define function slot-offset-i
    (slot-descriptor :: <slot-descriptor>, in-iclass :: <implementation-class>)
 => (offset :: <object> /* union(singleton(#f), <integer>) */,
     suboffset :: <object> /* union(singleton(#f), <integer>) */);
  case
    instance?(slot-descriptor, <repeated-slot-descriptor>) =>
      values(size(instance-slot-descriptors(in-iclass)), #f);
    instance?(slot-descriptor, <any-instance-slot-descriptor>) =>
       let v :: <simple-object-vector> = instance-slot-descriptors(in-iclass);
       let n :: <integer> = size(v);
       // Yeah, this looks just like the code for class slots, but they will
       // diverge when I handle packing and the like.  Class slots probably won't.
       local method loop1 (i :: <integer>)
               if (i == n)
                 values(#f, #f)
               else
                 let e :: <slot-descriptor> = vector-element(v, i);
                 if (getter=(e, slot-descriptor))
                   values(i, #f)
                 else
                   loop1(i + 1)
                 end if
               end if
             end method;
      loop1(0);
    instance?(slot-descriptor, <any-class-slot-descriptor>) =>
      let v :: <simple-object-vector> = class-slot-descriptors(in-iclass);
      let n :: <integer> = size(v);
      local method loop (i :: <integer>)
               if (i == n)
                 values(#f, #f)
               else
                 let e :: <slot-descriptor> = vector-element(v, i);
                 if (getter=(e, slot-descriptor))
                   values(i, #f)
                 else
                   loop(i + 1)
                 end if
               end if
             end method;
      loop(0);
    otherwise => error(make(<simple-slot-error>,
                            format-string: "Unanticipated type of slot descriptor %=",
                            format-arguments: list(slot-descriptor)));
  end case
end function;

define inline function slot-offset (sd :: <slot-descriptor>, c :: <class>)
  slot-offset-i(sd, class-implementation-class(c))
end function;


//// DYNAMIC ACCESSOR GLUE CODE

// Dynamic slot accessors have no IEP, so the XEP's are used to differentiate
// them.  The XEP code is in the runtime kernel, and does some of the basic
// function entry checks (number of argument, stack overflow, any kind of
// polling or other implementational trivia), then directly (tail)calls a
// corresponding glue routine from the set below.  The glue routine does the
// necessary type checking, and performs the access via slot-value and friends.
//
// The set of entrypoints below is chosen based on the distinctions possible
// solely from the slot descriptor, with the intent that compile-time method
// selection is done on the slot-value calls.  Further optimization is possible
// (e.g. separating byte vs. boxed-pointer repeated slot access, recognizing
// primary slots and not needing to do slot-value at all) but is unlikely to be
// of much overall use, since this code is only for when accessor methods are
// called via XEP.


define function %slotacc-single-Q-instance-getter
    (a :: <getter-method>, inst) => (value)
  let slotd :: <instance-slot-descriptor> = method-slot-descriptor(a);
  let owner :: <class> = slot-owner(slotd);
  if (instance?(inst, owner))
    slot-value(inst, slotd)
  else
    type-check-error(inst, owner)
  end if
end function;


define function %slotacc-single-Q-instance-setter
    (value, a :: <setter-method>, inst) => (value)
  let slotd :: <instance-slot-descriptor> = method-slot-descriptor(a);
  let owner :: <type> = slot-owner(slotd);
  /* Value stored is type-checked by slot-value-setter. */
  if (instance?(inst, owner))
    slot-value(inst, slotd) := value
  else
    type-check-error(inst, owner)
  end if
end function;


define function %slotacc-single-Q-class-getter
    (a :: <getter-method>, inst) => (value)
  let slotd :: <any-class-slot-descriptor> = method-slot-descriptor(a);
  let owner :: <class> = slot-owner(slotd);
  if (instance?(inst, owner))
    slot-value(inst, slotd)
  else
    type-check-error(inst, owner)
  end if
end function;

define function %slotacc-single-Q-class-setter
    (value, a :: <setter-method>, inst) => (value)
  let slotd :: <any-class-slot-descriptor> = method-slot-descriptor(a);
  let owner :: <type> = slot-owner(slotd);
  /* Value stored is type-checked by slot-value-setter. */
  if (instance?(inst, owner))
    slot-value(inst, slotd) := value
  else
    type-check-error(inst, owner)
  end if
end function;


define function %slotacc-repeated-instance-getter
    (a :: <repeated-getter-method>, inst, idx) => (value)
  if (instance?(idx, <integer>))
    let slotd :: <repeated-slot-descriptor> = method-slot-descriptor(a);
    let owner :: <class> = slot-owner(slotd);
    if (instance?(inst, owner))
      slot-value(inst, slotd)
    else
      type-check-error(inst, owner)
    end if
  else
    type-check-error(idx, <integer>)
  end if
end function;

define function %slotacc-repeated-instance-setter
    (value, a :: <repeated-setter-method>, inst, idx) => (value)
  if (instance?(idx, <integer>))
    let slotd :: <repeated-slot-descriptor> = method-slot-descriptor(a);
    let owner :: <type> = slot-owner(slotd);
    /* Value stored is type-checked by repeated-slot-value-setter. */
    if (instance?(inst, owner))
      slot-value(inst, slotd) := value
    else
      type-check-error(inst, owner)
    end if
  else
    type-check-error(idx, <integer>)
  end if
end function;


define method function-number-required
    (f :: <lambda>) => (n :: <integer>)
  signature-number-required(function-signature(f))
end method;

define method function-number-required
    (f :: <generic-function>) => (n :: <integer>)
  signature-number-required(function-signature(f))
end method;

define method function-number-required
    (f :: <accessor-method>) => (n :: <integer>)
  if (instance?(f, <repeated-accessor-method>))
    if (instance?(f, <setter-accessor-method>)) 3 else 2 end;
  else
    if (instance?(f, <setter-accessor-method>)) 2 else 1 end;
  end if
end method;


// define method sealed-domain? (f :: <accessor-method>) => (well? :: <boolean>)
//   slot-method-sealed?(method-slot-descriptor(f))
// end method;



define method lazy-subtype?(type1 :: <type>, type2 :: <type>)
  => (boolean :: <boolean>)
  if (type2 == <object>)
    #t
  else
    subtype?(type1, type2);
  end if;
end method;


define method congruent? (g :: <generic-function>, m :: <accessor-method>)
 => (congruent? :: <boolean>, reason);

  block (return)
    local method fail (reason)
            return(#f, reason)
          end method fail;

    let gsig :: <signature> = function-signature(g);
    let sd :: <slot-descriptor> = method-slot-descriptor(m);

    // --- required arguments ---
    // They have the same number of required arguments.
    //
    // Each of the method's parameter specializers is a subtype of the
    // corresponding parameter specializer of the generic function.

    let greq :: <simple-object-vector> = gsig.signature-required;
    let gsiz :: <integer> = gsig.signature-number-required;
    let msiz :: <integer> = function-number-required(m);

    if (gsiz ~== msiz)
      fail($required-argument-count);
    end if;
    for (i :: <integer> from 0 below gsiz)
      unless (lazy-subtype?(%method-specializer(m, i), vector-element(greq, i)))
        fail($required-argument-type);
      end unless
    end for;

    // --- optional arguments ---
    case
      gsig.signature-key? => fail($not-both-keyword);
      gsig.signature-rest? => fail($not-both-variable);
    end case;

    // --- value declarations ---
    let grestv? = gsig.signature-rest-value?;
    let gvals :: <simple-object-vector> = gsig.signature-values;
    let mval :: <type> = sd.slot-type;
    let gvsiz :: <integer> = gsig.signature-number-values;
    if (gvsiz > 1)
      fail($required-values-count-too-small)
    elseif (gvsiz == 1)
      unless (lazy-subtype?(mval, vector-element(gvals, 0)))
        fail($required-values-type)
      end unless
    elseif (~lazy-subtype?(mval, gsig.signature-rest-value))
      fail($required-values-type)
    end if;

    values(#t, #"congruent")
  end block;

end method congruent?;



define method type-complete? (m :: <accessor-method>) => (well? :: <boolean>)
  let sd :: <slot-descriptor> = method-slot-descriptor(m);
  type-complete?(slot-owner(sd)) & type-complete?(slot-type(sd))
end method;


define method recompute-type-complete! (m :: <accessor-method>) => (well? :: <boolean>)
  let sd :: <slot-descriptor> = method-slot-descriptor(m);
  let a1 = recompute-type-complete!(slot-owner(sd));
  let a2 = recompute-type-complete!(slot-type(sd));
  a1 & a2
end method;


define method map-congruency-classes (f :: <function>, m :: <accessor-method>) => ()
  let sd :: <slot-descriptor> = method-slot-descriptor(m);
  map-congruency-classes(f, slot-owner(sd));
  map-congruency-classes(f, slot-type(sd));
end method;


define method reduce-incomplete-classes (f :: <function>, m :: <accessor-method>, ans)
 => (ans)
  let sd :: <slot-descriptor> = method-slot-descriptor(m);
  reduce-incomplete-classes(f, slot-owner(sd), reduce-incomplete-classes(f, slot-type(sd), ans))
end method;
