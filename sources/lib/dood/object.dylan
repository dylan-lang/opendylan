Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// ATTACHING

define inline method dood-segment-initialized? 
    (segment :: <dood-segment-state>) => (well? :: <boolean>)
  dood-segment-free-address(segment) > 0
end method;

define inline method dood-lookup-segment-state-by-id
    (dood :: <dood>, id :: <dood-segment-id>) => (res :: <dood-segment-state>)
  dood-segment-states(dood)[id]
end method;

define method dood-slow-lookup-segment
    (dood :: <dood>, class :: <class>) => (res :: false-or(<dood-segment>))
  let segment :: false-or(<dood-segment>)
    = block (return)
        for (segment in dood-segments(dood))
          when (dood-segment-instance?(segment, class))
            return(segment)
          end when;
        end for;
        #f
      end block;
  segment
end method;

define method dood-lookup-segment-state
    (dood :: <dood>, dood-class :: <dood-class>, object)
 => (res :: <dood-segment-state>)
  let segment :: <dood-segment>
    = dood-class-segment(dood-class)
        | dood-current-segment(dood);
  dood-current-segment(dood) := segment;
  let segment-state :: <dood-segment-state>
    = dood-lookup-segment-state-by-id(dood, dood-segment-id(segment));
  segment-state
end method;

define macro with-segment-excursion
  { with-segment-excursion (?dood:expression, ?segment:expression) ?:body end }
     => { let saved-segment = dood-current-segment(?dood);
          dood-current-segment(?dood) := ?segment;
          ?body;
          dood-current-segment(?dood) := saved-segment }
end macro;

define inline method dood-address-page-mask 
    (dood :: <dood>) => (res :: <integer>)
  ash(dood-address-buffer-mask(), -2)
end method;

define inline method dood-page-size
    (dood :: <dood>) => (res :: <integer>)
  ash(dood-buffer-size(), -2)
end method;

define inline method dood-address-page-bit-offset 
    (dood :: <dood>) => (res :: <integer>)
  dood-address-buffer-bit-offset() - 2
end method;

define inline method dood-fits-in-page? 
    (dood :: <dood>, address :: <address>, size :: <integer>) => (well? :: <boolean>)
  let fits?
    = size < logand(address + size, dood-address-page-mask(dood));
  // format-out("FITS? %= ADDR %= SIZE %= MASK %=\n", 
  //            fits?, address, size, dood-address-page-mask(dood));
  fits?
end method;

define inline method dood-number-pages 
    (dood :: <dood>, size :: <integer>) => (res :: <integer>)
  let number-pages
    = ash(size, - dood-address-page-bit-offset(dood))
        + if (logand(size, dood-address-page-mask(dood)) > 0) 1 else 0 end;
  // format-out("NUMBER-PAGES %= SIZE %= OFFSET %= OVERFLOW %=\n",
  //            number-pages, size, dood-address-page-bit-offset(dood),
  //            logand(size, dood-address-page-mask(dood)));
  number-pages
end method;

define inline method dood-allocate-pages
    (dood :: <dood>, n :: <integer>) => (address :: <address>)
  let address = dood.dood-free-address;
  dood-free-address(dood) := address + n * dood-page-size(dood);
  address
end method;

define inline method dood-segment-fits-in-page? 
    (dood :: <dood>, segment :: <dood-segment-state>, size :: <integer>) 
 => (well? :: <boolean>)
  dood-fits-in-page?(dood, dood-segment-free-address(segment), size)
end method;

define method dood-allocate-in
    (dood :: <dood>, segment :: <dood-segment-state>, size :: <integer>) 
 => (address :: <address>)
  let address
    = if (dood-segment-initialized?(segment)
            & dood-segment-fits-in-page?(dood, segment, size))
        segment.dood-segment-free-address
      else
        let address = dood-allocate-pages(dood, dood-number-pages(dood, size));
        // format-out("ALLOCATING %= PAGES @ %d IN %s\n", 
        //         dood-number-pages(dood, size), address,
        //         dood-segment-name(dood-segment-state-segment(segment)));
        address
      end if;
  dood-segment-free-address(segment) := address + size;
  address
end method;

define method dood-allocate
    (dood :: <dood>, object, dood-class :: <dood-class>, size :: <integer>) 
 => (address :: <address>)
  let segment = dood-lookup-segment-state(dood, dood-class, object);
  let address = dood-allocate-in(dood, segment, size);
  // format-out("ALLOCATING %= (%d) @ %d IN %s\n", 
  //         object-class(object), size, address, 
  //         dood-segment-name(dood-segment-state-segment(segment)));
  address
end method;

define function dood-compute-standard-instance-size
    (dood :: <dood>, class :: <class>) => (address :: <address>)
  1 + size(dood-kept-slot-descriptors(dood, class))
end function;

define method dood-compute-instance-size
    (dood :: <dood>, class :: <class>) => (address :: <address>)
  dood-compute-standard-instance-size(dood, class)
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object == <single-float>) => (address :: <address>)
  1 + 1
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object == <double-float>) => (address :: <address>)
  1 + 1 + 1
end method;

define method dood-allocate-instance 
    (dood :: <dood>, object) => (address :: <address>)
  dood-format("ALLOCATING DOOD INSTANCE %=\n", object);
  let class      = object-class(object);
  let dood-class = dood-class(dood, class);
  let disk-size  = dood-instance-size-using-class(dood, object, dood-class);
  dood-allocate(dood, object, dood-class, disk-size);
end method;

define method attach-object 
    (dood :: <dood>, memory-object, disk-object) => (address :: <address>)
  let address = dood-allocate-instance(dood, disk-object);
  dood-format("ATTACHING INSTANCE %= -> %=\n", memory-object, disk-object);
  unless (disk-object == memory-object)
    dood-register-object(dood, disk-object, address);
  end unless;
  dood-register-object(dood, memory-object, address);
  address
end method;

/*
define method attach (dood :: <dood>, object) => (address)
  let disk-object = dood-disk-object(dood, object);
  attach-object(dood, object, disk-object);
end method;
*/

// define function dood-address! (dood :: <dood>, object)
//   dood-address(dood, object) | attach(dood, object)
// end function;

//// READING

define inline function mark-lazy-slot-using
    (dood :: <dood>, x, address :: <address>, 
     slotd :: <dood-slot-descriptor>, offset :: <integer>, force? :: <boolean>)
 => ()
  when (force? | ~lazy-value?(dood-slot-value(x, slotd)))
    let (found?, address-or-object) = shallow-read-object(dood);
    dood-slot-value(x, slotd) 
      := if (found?)
           address-or-object
         else 
           make-slot-value-proxy(dood, address-or-object, offset, slotd);
         end if;
  end when;
end function;

define inline function mark-lazy-slots-using
    (dood :: <dood>, x, address :: <address>,
     lazy-slotds :: <dood-slot-sequence-type>, 
     deep-slotds :: <dood-slot-sequence-type>,
     #key force?) 
 => (y)
  for (slotd in lazy-slotds,
       offset :: <integer> from size(deep-slotds))
    mark-lazy-slot-using(dood, x, address, slotd, offset, force?);
  end for;
  x
end function;

define inline function mark-lazy-slots-at 
    (dood :: <dood>, x, address :: <address>, #key force? = #t) => (y)
  let (lazy-slotds, weak-slotds, deep-slotds)
    = dood-all-slot-descriptors(dood, object-class(x));
  ignore(weak-slotds);
  mark-lazy-slots-using
    (dood, x, address, lazy-slotds, deep-slotds, force?: force?);
end function;

define function mark-lazy-slots (dood :: <dood>, x) => (y)
  mark-lazy-slots-at(dood, x, dood-address(dood, x));
end function;

define thread variable *print-depth* = 0;

define function depth-format-out (string, #rest args)
  if (*trace-allocation?*)
    for (i from 0 below *print-depth*)
      format-out("  ")
    end for;
    block ()
      apply(format-out, string, args)
    exception (<error>)
      format-out("\n");
    end block;
  end if;
end function;

define variable *watchpoint-class* = #f;

define method dood-watchpoint-class-setter (debug-name)
  *watchpoint-class* := debug-name;
end method;

define method dood-watchpoint-class ()
  *watchpoint-class*
end method;

define variable *watchpoint-dood* = #f;

define method dood-watchpoint-dood-setter (debug-name)
  *watchpoint-dood* := as(<symbol>, debug-name);
end method;

define method dood-watchpoint-dood ()
  *watchpoint-dood*
end method;

define constant $queue-deep-slots? = #f;

define inline function dood-read-slot!
    (dood :: <dood>, x, index, work :: <dood-queue>, slot-value-setter :: <function>)
  if ($queue-deep-slots?)
    let (found?, address-or-object) = shallow-read-object(dood);
    if (found?)
      slot-value(x, index) := address-or-object;
    else 
      dood-queue-push-last(work, index);
      dood-queue-push-last(work, address-or-object);
    end if;
  else
    slot-value(x, index) := read-object(dood);
  end if;
end function;

define inline function dood-do-slot-work
    (dood :: <dood>, x, work :: <dood-queue>, 
     slot-value-setter :: <function>, next-in :: <integer>)
  when ($queue-deep-slots?)
    let index   = dood-queue-pop(work);
    let address = dood-queue-pop(work);
    with-saved-queue-frame (work, next-in)
      slot-value(x, index) := read-pointer(dood, address);
    end with-saved-queue-frame;
  end when;
end function;

define inline function dood-do-work
    (dood :: <dood>, x, work :: <dood-queue>, 
     required-out :: <integer>, repeated-out :: <integer>, final-out :: <integer>)
  when ($queue-deep-slots?)
    let final-out = dood-queue-in(work);
    dood-queue-out(work) := required-out;
    until (dood-queue-out(work) == repeated-out)
      dood-do-slot-work(dood, x, work, dood-slot-value-setter, final-out);
    end until;
    until (dood-queue-out(work) == final-out)
      dood-do-slot-work(dood, x, work, element-setter, final-out);
    end until;
    dood-queue-in(work)  := required-out;
    dood-queue-out(work) := required-out;
  end when;
end function;

define constant $tracing? = #f;

define function dood-read-object-of-at
    (dood :: <dood>, class :: <class>, address :: <address>)
  // TODO: ASSUMES SIZE IS FIRST KEPT SLOT
  if ($tracing? & *trace-allocation?*)
    depth-format-out("READING A %=\n", class);
    *print-depth* := *print-depth* + 1;
  end if;
  let work = dood-work(dood);
  // dynamic-bind (*print-depth* = *print-depth* + 1)
  let (lazy-slotds, weak-slotds, deep-slotds, repeated-slot?, repeated-byte-slot?) 
    = dood-all-slot-descriptors(dood, class);
  let size = if (repeated-slot?) read-object(dood) end;
  let object = allocate-object(class, size);
  unless (instance?(object, <dood-proxy>)) // DONT WANT THIS IN CACHE
    dood-register-read-object(dood, object, address); 
  end unless;
  let required-out = dood-queue-in(work);
  for (slotd :: <dood-slot-descriptor> in deep-slotds)
    dood-read-slot!(dood, object, slotd, work, dood-slot-value-setter);
  end for;
  let repeated-out = dood-queue-in(work);
  if (repeated-slot?)
    if (repeated-byte-slot?)
      dood-read-string-into!(dood, size, object)
    else
       for (i :: <integer> from 0 below size)
         dood-read-slot!(dood, object, i, work, element-setter);
      end for;
    end if;
  end if;
  mark-lazy-slots-using(dood, object, address, lazy-slotds, deep-slotds);

  dood-do-work(dood, object, work, required-out, repeated-out, dood-queue-in(work));

  for (slotd/default :: <dood-defaulted-descriptor> in weak-slotds)
    let slotd         = dood-default-slot-descriptor(slotd/default);
    let default-thunk = dood-default-thunk(slotd/default);
    dood-slot-value(object, slotd) := default-thunk(object);
  end for;


  // end dynamic-bind;
  if ($tracing? & *trace-allocation?*)
    *print-depth* := *print-depth* - 1;
    depth-format-out("READ %=\n", object);
  end if;
  if ($tracing? & *watchpoint-class* & debug-name(class) = *watchpoint-class*)
    if (~*watchpoint-dood* | dood-name(dood) = *watchpoint-dood*)
      break("FOUND %=\n", object);
    end if;
  end if;
  object
end function;

define method read-object-using-class-at
    (dood :: <dood>, class == <machine-word>, address :: <address>)
 => (object)
  dood-read-machine-word(dood)
end method;

define method read-object-using-class-at
    (dood :: <dood>, class == <single-float>, address :: <address>)
 => (object)
  encode-single-float(dood-read-machine-word(dood))
end method;

define method read-object-using-class-at
    (dood :: <dood>, class == <double-float>, address :: <address>)
 => (object)
  let low  = dood-read-machine-word(dood);
  let high = dood-read-machine-word(dood);
  encode-double-float(low, high)
end method;

define method read-object-using-class-at
    (dood :: <dood>, class :: <class>, address :: <address>)
 => (object)
  let object = dood-read-object-of-at(dood, class, address);
  // dood-weak-reinitialize(object);
  dood-reinitialize(dood, object);
  object
end method;

define constant $address-not-found :: <list> = list("ADDRESS NOT FOUND");

define generic read-object-using-class-at
  (dood :: <dood>, class :: <class>, address :: <address>) => (res);

define function read-address
    (dood :: <dood>, address :: <address>) => (object)
  with-saved-position (dood)
    // ---*** This emits a warning here because read-object-at and
    // friends get inlined. The warning is because maybe-read-pointer
    // can return the default value (here: #f), or an <integer> or
    // <byte-character>. I don't see a good way to make this not
    // warn currently.
    let class :: <class> = read-object-at(dood, address);
    audit(dood, "%dT%d%s\n", address, debug-name(class));
    read-object-using-class-at (dood, class, address);
  end with-saved-position
end function;

define function read-pair
    (dood :: <dood>, address :: <address>) => (object)
  with-saved-position (dood)
    dood-position(dood) := address;
    audit(dood, "%dT%d%s\n", address, #"<pair>");
    read-object-using-class-at (dood, <pair>, address);
  end with-saved-position
end function;

define inline function maybe-read-pointer
    (dood :: <dood>, pointer :: <pointer>, read? :: <boolean>, default) => (object)
  let address = untag(pointer);
  case
    $tag-pairs? & pair?(pointer) =>
      let value = dood-object(dood, address, default: $address-not-found);
      if (value == $address-not-found)
        if (read?)
          read-pair(dood, address);
        else 
          default
        end if
      else
        value
      end if;
    address?(pointer) =>
      let value = dood-object(dood, address, default: $address-not-found);
      if (value == $address-not-found)
        if (read?)
          read-address(dood, address);
        else 
          default
        end if
      else
        value
      end if;
    integer?(pointer) =>
      address-as-integer(address);
    byte-character?(pointer) =>
      as(<byte-character>, address);
  end case;
end function;

define inline function read-pointer
    (dood :: <dood>, pointer :: <pointer>) => (object)
  maybe-read-pointer(dood, pointer, #t, #f)
end function;

define inline function peek-pointer
    (dood :: <dood>, pointer :: <pointer>, default) => (object)
  maybe-read-pointer(dood, pointer, #f, default)
end function;

define inline function read-object-at 
    (dood :: <dood>, address :: <address>) => (object)
  read-pointer(dood, dood-read-at(dood, address))
end function;

define function read-object (dood :: <dood>) => (object)
  read-pointer(dood, dood-read(dood))
end function;

define function shallow-read-object (dood :: <dood>) => (found?, object-or-address)
  let address = dood-read(dood);
  let object  = peek-pointer(dood, address, $unfound);
  if (found?(object))
    values(#t, object)
  else 
    values(#f, address)
  end if;
end function;

//// REINITIALIZATION

// define open generic dood-weak-reinitialize (object) => ();

// define method dood-weak-reinitialize (object) => ()
// end method;

define open generic dood-reinitialize (dood :: <dood>, object) => ();

define method dood-reinitialize (dood :: <dood>, object) => ()
  object
end method;

//// FLUSHING

define method dood-flush-lazy-slots (dood :: <dood>, object) => ()
  mark-lazy-slots(dood, object);
end method;
