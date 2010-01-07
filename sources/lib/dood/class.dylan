Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <dood-slot-descriptor>  = <walker-slot-descriptor>;

define constant dood-slot-value         = walker-slot-value; 
define constant dood-slot-value-setter  = walker-slot-value-setter; 
define constant as-dood-slot-descriptor = as-walker-slot-descriptor;

define constant <dood-slot-sequence-type>
  = <walker-slot-sequence-type>;

define constant allocate-object         = walker-allocate-object;

define class <dood-class> (<object>)
  slot dood-class-instance-size :: <integer>;
  constant slot dood-class-repeated-slot? :: <boolean>,
    required-init-keyword: repeated-slot?:;
  constant slot dood-class-repeated-byte-slot? :: <boolean>,
    required-init-keyword: repeated-byte-slot?:;

  constant slot dood-class-lazy-slot-descriptors :: <dood-slot-sequence-type>, 
    required-init-keyword: lazy-slot-descriptors:;
  constant slot dood-class-weak-slot-descriptors :: <dood-defaulted-slot-sequence-type>, 
    required-init-keyword: weak-slot-descriptors:;
  constant slot dood-class-deep-slot-descriptors :: <dood-slot-sequence-type>, 
    required-init-keyword: deep-slot-descriptors:;
  slot dood-class-kept-slot-descriptors :: <dood-slot-sequence-type>;
  constant slot dood-class-segment :: false-or(<dood-segment>) = #f,
    init-keyword: segment:;
end class;

define method initialize
   (dood-class :: <dood-class>, #rest all-keys, 
    #key lazy-slot-descriptors :: <dood-slot-sequence-type>, 
         deep-slot-descriptors :: <dood-slot-sequence-type>)
  next-method();
  dood-class-kept-slot-descriptors(dood-class) 
    := concatenate(lazy-slot-descriptors, deep-slot-descriptors);
end method;

define sealed domain initialize (<dood-class>);
define sealed domain make (singleton(<dood-class>));

define open generic dood-lazy-getters (class :: <class>) => (res :: <list>);
define open generic dood-weak-getters (class :: <class>) => (res :: <list>);

define method dood-lazy-getters (class :: <class>) => (res :: <list>)
  #()
end method;

define method dood-weak-getters (class :: <class>) => (res :: <list>)
  #()
end method;

define method dood-compute-lazy-slot-descriptors 
    (class :: <class>) => (res :: <dood-slot-sequence-type>)
  let lazy-getters = dood-lazy-getters(class);
  collecting (as <dood-slot-sequence-type>)
    for (sd in dood-instance-slot-descriptors(class))
      when (member?(slot-getter(sd), lazy-getters))
	collect(as-dood-slot-descriptor(class, sd))
      end when;
    end for;
  end collecting;
end method;

define constant make-dood-defaulted-descriptor 
  = make-walker-defaulted-descriptor;
define constant <dood-defaulted-descriptor>
  = <walker-defaulted-descriptor>;
//define constant dood-defaulted-getter?
//  = walker-defaulted-getter?;
define constant dood-default-getter              
  = walker-default-getter;
define constant dood-default-thunk               
  = walker-default-thunk;
define constant dood-default-slot-descriptor     
  = walker-default-slot-descriptor;

define constant <dood-defaulted-slot-sequence-type> 
  = <walker-defaulted-slot-sequence-type>;

define method dood-compute-weak-slot-descriptors 
    (class :: <class>) => (res :: <dood-defaulted-slot-sequence-type>)
  let weak-getters = dood-weak-getters(class);
  collecting (as <dood-defaulted-slot-sequence-type>)
    for (sd in dood-instance-slot-descriptors(class))
      for (weak-getter in weak-getters)
	if (slot-getter(sd) == dood-default-getter(weak-getter))
          let thunk = dood-default-thunk(weak-getter);
          if (thunk)
  	    collect(make-dood-defaulted-descriptor
                     (as-dood-slot-descriptor(class, sd), thunk));
          end if;
	end if;
      end for;
    end for;
  end collecting;
end method;

define method dood-base-deep-slot-descriptors
    (class :: <class>) => (slotds :: <slot-sequence-type>)
  let slotds         
    = as(<slot-sequence-type>, dood-instance-slot-descriptors(class));
  let repeated-slotd
    = repeated-slot-descriptor(class);
  if (repeated-slotd)
    // remove size slotd
    let size-slotd = size-slot-descriptor(repeated-slotd);
    choose(method (x) x ~== size-slotd end, slotds);
  else
    slotds
  end if;
end method;

define method dood-compute-deep-slot-descriptors 
    (class :: <class>) => (res :: <dood-slot-sequence-type>)
  let weak-getters = map(dood-default-getter, dood-weak-getters(class));
  let lazy-getters = dood-lazy-getters(class);
  collecting (as <dood-slot-sequence-type>)
    for (sd in dood-base-deep-slot-descriptors(class))
      when (~member?(slot-getter(sd), lazy-getters) & ~member?(slot-getter(sd), weak-getters))
	collect(as-dood-slot-descriptor(class, sd))
      end when;
    end for;
  end collecting;
end method;

define generic dood-compute-instance-size 
  (dood :: <dood>, class :: <class>) => (res :: <integer>);

define function dood-class
    (dood :: <dood>, class :: <class>) => (res :: <dood-class>)
  element(dood-classes(dood), class, default: #f) |
    (begin 
       let dood-class :: <dood-class>
         = make(<dood-class>, 
		repeated-slot?: 
		  if (dood-repeated-slot?(dood, class)) #t else #f end,
		repeated-byte-slot?: 
		  dood-repeated-byte-slot?(dood, class),
		lazy-slot-descriptors:
		  dood-compute-lazy-slot-descriptors(class),
		weak-slot-descriptors:
		  dood-compute-weak-slot-descriptors(class),
		deep-slot-descriptors:
		  dood-compute-deep-slot-descriptors(class),
		segment:
		  dood-slow-lookup-segment(dood, class));
       element(dood-classes(dood), class)
         := dood-class;
       dood-class-instance-size(dood-class)
         := dood-compute-instance-size(dood, class);
       dood-class
     end)
end function;

define function dood-all-slot-descriptors
    (dood :: <dood>, class :: <class>)
 => (lazy-slotds :: <dood-slot-sequence-type>,
     weak-slotds :: <dood-defaulted-slot-sequence-type>,
     deep-slotds :: <dood-slot-sequence-type>,
     repeated-slot? :: <boolean>,
     repeated-byte-slot? :: <boolean>)
  let dood-class = dood-class(dood, class);
  values(dood-class-lazy-slot-descriptors(dood-class),
	 dood-class-weak-slot-descriptors(dood-class),
	 dood-class-deep-slot-descriptors(dood-class),
	 dood-class-repeated-slot?(dood-class),
	 dood-class-repeated-byte-slot?(dood-class))
end function;

define inline function dood-slot-descriptors-of
    (dood :: <dood>, class :: <class>, access :: <function>)
 => (res)
  access(dood-class(dood, class))
end function;

/*
define function dood-lazy-slot-descriptors
    (dood :: <dood>, class :: <class>) => (res :: <dood-slot-sequence-type>)
  dood-slot-descriptors-of(dood, class, dood-class-lazy-slot-descriptors)
end function;

define function dood-weak-slot-descriptors 
    (dood :: <dood>, class :: <class>) => (res :: <dood-defaulted-slot-sequence-type>)
  dood-slot-descriptors-of(dood, class, dood-class-weak-slot-descriptors)
end function;

define function dood-deep-slot-descriptors 
    (dood :: <dood>, class :: <class>) => (res :: <dood-slot-sequence-type>)
  dood-slot-descriptors-of(dood, class, dood-class-deep-slot-descriptors)
end function;
*/

define function dood-kept-slot-descriptors 
    (dood :: <dood>, class :: <class>) => (res :: <dood-slot-sequence-type>)
  dood-slot-descriptors-of(dood, class, dood-class-kept-slot-descriptors)
end function;

define generic dood-repeated-size 
  (dood :: <dood>, object) => (res :: <integer>);

define method dood-repeated-size (dood :: <dood>, object) => (res :: <integer>)
  size(object)
end method;

define inline function dood-instance-size-using-class 
    (dood :: <dood>, object, dood-class :: <dood-class>) => (res :: <integer>)
  dood-class-instance-size(dood-class)
    + if (dood-class-repeated-slot?(dood-class))
        let size = dood-repeated-size(dood, object);
	1 + if (dood-class-repeated-byte-slot?(dood-class))
	      bytes-to-words(size)
	    else
	      size
	    end if 
      else
	0
      end if
end function;

define function dood-instance-size (dood :: <dood>, object) => (res :: <integer>)
  let class      = object-class(object);
  let dood-class = dood-class(dood, class);
  dood-instance-size-using-class(dood, object, dood-class)
end function;
