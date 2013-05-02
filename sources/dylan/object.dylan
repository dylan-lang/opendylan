Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <object> ... end;

define open generic type-for-copy (object) => (result :: <type>);

define open generic shallow-copy (object);

define inline method initialize (instance, #key, #all-keys) => ()
end method initialize;

define method slot-initialized?
    (instance, getter :: <function>) => (result :: <boolean>);
  let descriptor = slot-descriptor(instance, getter);
  if (descriptor)
    slot-initialized?(instance, descriptor)
  else
    error("Slot-descriptor not found in object %s for getter %s",
          instance,
          getter)
  end if
end method slot-initialized?;

define method as (class :: <type>, object) => object;
  if (instance?(object, class))
    object
  else
    error("Object %= cannot be converted to class %=", object, class)
  end if
end method as;

define method type-for-copy (object) => (class :: <class>);
  object.object-class
end method type-for-copy;

/*
(define-method repeated-storage-size (instance)
  (bind ((class (object-class instance))
         (repeated-slot-descriptor (repeated-slot-descriptor class)))
    (if repeated-slot-descriptor
        (bind ((size ((getter (size-slot-descriptor repeated-slot-descriptor))
                      instance)))
          (if (repeated-byte-slot? repeated-slot-descriptor) size (* size 4)))
        0)))

(define-method slot-storage-size (instance)
  (bind ((class (object-class instance)))
    (+ (instance-storage-size class) (repeated-storage-size instance))))
*/

/// !@#$ needs to be redone when storage-sizes are reconciled

/*
define method replace-slots! (dst, src)
  for (slot-descriptor in instance-slot-descriptors(src.object-class))
    if (member?(slot-descriptor.slot-allocation, #(#"instance", #"constant")))
      slot-value(dst, slot-descriptor) := slot-value(src, slot-descriptor)
    end if;
  end for;
  dst
end method replace-slots!;
*/

/*
// not supposed to be predefined for non-collections
define method shallow-copy (object)
  let new-object = allocate-instance(object.object-class, #[]);
  replace-slots!(new-object, object)
end method shallow-copy;
*/

/// !@#$ this is more like what replace-slots! should be like

/*
define method copy-simple-instance (object) => object;
  let class :: <class> = object.object-class;
  let new-object = class.system-allocate-simple-instance;
  let instance-storage-size :: <integer> = class.instance-storage-size;
  for (offset :: <integer> from 0 below instance-storage-size)
    slot-element(new-object, offset) := slot-element(object, offset);
  end for;
  new-object
end method copy-simple-instance;
*/

define method \= (object-1, object-2) => (equal? :: <boolean>)
  object-1 == object-2
end method \=;

define open generic debug-name (object :: <object>);
define open generic debug-name-setter (name :: <object>, object :: <object>);

define method debug-name (object)
  #f                                // !@#$ SHOULD FILL THIS IN PROPERLY
end method debug-name;

define open generic size
    (object :: <object>);

define open generic size-setter
  (size :: <object>, object :: <object>);

define open generic empty?
  (object :: <object>) => (result :: <boolean>);

define open generic as-lowercase!
    (object :: <object>) => (result :: <object>);

define open generic as-uppercase!
    (object :: <object>) => (result :: <object>);
