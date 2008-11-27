Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $queue-walk?      = #f;
define constant $queue-lazy-walk? = #t | $queue-walk?;

define open generic dood-disk-object
    (dood :: <dood>, object) => (disk-object);

// define sealed generic dood-disk-pointer+object
//     (dood :: <dood>, object) => (pointer :: <pointer>, disk-object);

// define method dood-disk-pointer+object
//     (dood :: <dood>, object :: <integer>)
//  => (pointer :: <pointer>, disk-object)
//   dood-integer-disk-pointer+object(dood, object);
// end method;

// define method dood-disk-pointer+object
//     (dood :: <dood>, object :: <byte-character>)
//  => (pointer :: <pointer>, disk-object)
//   dood-character-disk-pointer+object(dood, object)
// end method;

define inline function dood-indirect-disk-pointer+object 
    (dood :: <dood>, info :: <walk-info>, object) 
 => (pointer :: <pointer>, disk-object, found-address)
  let disk-object = dood-disk-object(dood, object);
  let found-address
    = (~walk-info-batch?(info) | disk-object ~== object)
        & dood-address(dood, disk-object);
  let address = found-address | attach-object(dood, object, disk-object);
  values(tag-as-address(disk-object, address), disk-object, found-address)
end function;

// define method dood-disk-pointer+object 
//     (dood :: <dood>, object) => (pointer :: <pointer>, disk-object)
//   dood-indirect-disk-pointer+object(dood, object)  
// end method;

define function dood-disk-pointer+object 
    (dood :: <dood>, object) => (pointer :: <pointer>, disk-object)
  let class :: <class> = object-class(object);
  if (class == <integer>)
    let object :: <integer> = object;
    dood-integer-disk-pointer+object(dood, object);
  elseif (class == <byte-character>)
    let object :: <byte-character> = object;
    dood-character-disk-pointer+object(dood, object);
  else
    dood-indirect-disk-pointer+object(dood, $default-walk-info, object);
  end if;
end function;

define inline function disk-pointer
    (dood :: <dood>, object) => (pointer :: <pointer>)
  dood-disk-pointer+object(dood, object)
end function;

// define constant <dood-queue> = <stretchy-vector>;

define inline function dood-maybe-register-parent 
    (dood :: <dood>, info :: <walk-info>, object, parent)
  when (walk-info-parents?(info) & parent)
    // when (*walk-progress-function* == $default-walk-progress-function)
    //   format-out("   %= CONTAINS %=\n", parent, object);
    // end when;
    dood-register-walked-object(dood, object);
    dood-back-pointers(dood)[object] := parent;
  end when;
end function;

define function dood-walk-indirect-object 
    (dood :: <dood>, info :: <walk-info>, parent, object) 
 => (res :: <pointer>)
  let (pointer, disk-object, found-address)
    = dood-indirect-disk-pointer+object(dood, info, object);
  dood-register-walked-pointer(dood, object, disk-object, pointer);
  dood-maybe-register-parent(dood, info, object, parent);
  when (*watchpoint-class* & debug-name(object-class(object)) == *watchpoint-class*)
    when (~*watchpoint-dood* | dood-name(dood) = *watchpoint-dood*)
      break("FOUND %=\n", object);
    end when;
  end when;
  unless (found-address)
    let saved-position = dood-position(dood);
    walk-object(dood, info, disk-object, untag(pointer));
    dood-position(dood) := saved-position;
  end unless;
  pointer
end function;

// define constant object-kind          = object-class;
// define constant $integer-kind        = <integer>;
// define constant $byte-character-kind = <byte-character>;

define constant $lazy-pointer = tag-as-address(#f, 0);

define inline function maybe-walk-object
    (dood :: <dood>, info :: <walk-info>, parent, object, queue?)
 => (pointer :: <pointer>)
  let object = dood-standard-object(dood, object);
  case
    instance?(object, <integer>) & small-integer?(object) =>
      let object :: <integer> = object; // TYPE ONLY
      tag-as-integer(object);
    instance?(object, <byte-character>) =>
      let object :: <byte-character> = object; // TYPE ONLY
      dood-character-disk-pointer+object(dood, object);
    otherwise =>
      let maybe-address = dood-walked-address-using-table(dood, object);
      if (maybe-address)
	let address :: <address> = maybe-address;  // TYPE ONLY
	let disk-object
	  = if ($tag-pairs? & instance?(object, <pair>)) 
	      lookup-proxy(dood, object) | object;
	    else
	      object
	    end if;
	tag-as-address(disk-object, address)
      elseif (queue?)
	$lazy-pointer
      else 
	dood-walk-indirect-object(dood, info, parent, object)
      end if;
  end case
end function;

define inline function walk-slot
    (dood :: <dood>, info :: <walk-info>, parent, object, #key queue? = $queue-walk?)
  let pointer = maybe-walk-object(dood, info, parent, object, queue?);
  let commit? = walk-info-commit?(info);
  when (pointer == $lazy-pointer)
    dood-queue-push-last(dood-walk-queue(dood), object);
    dood-queue-push-last(dood-walk-queue(dood), parent);
    dood-queue-push-last(dood-walk-queue(dood), dood-current-segment(dood));
    when (commit?)
      dood-queue-push-last(dood-walk-queue(dood), dood-position(dood));
    end when;
  end when;
  when (commit?)
    dood-write(dood, pointer);
  end when;
end function;

define constant $default-walk-info = make(<walk-info>);

define inline function walked-pointer
    (dood :: <dood>, object) => (pointer :: false-or(<pointer>))
  maybe-walk-object(dood, $default-walk-info, #f, object, #f)
end function;

define generic walk-slots (dood :: <dood>, info :: <walk-info>, object);

define inline function dood-lazy-slot-value-using 
    (dood :: <dood>, object, 
     slotd :: <dood-slot-descriptor>, offset :: <integer>, 
     force?, flush?, commit?,
     force-proxy :: <function>, mark-proxy :: <function>)
 => (value, lazy?)
  let value = dood-slot-value(object, slotd);
  if (lazy-value?(value))
    if (commit? | force?)
      values(force-proxy(object, value), #t)
    else
      values(value, #t)
    end if
  else 
    if (flush?)
      let address :: <address> = dood-address(dood, object);
      // format-out("FLUSHING %= @ %= OFFSET %=\n", 
      //            object-class(object), address, offset);
      mark-proxy(dood, object, address, slotd, offset, #t);
    end if;
    values(value, #f)
  end if;
end function;

define inline function walk-lazy-slot 
    (dood :: <dood>, info :: <walk-info>, object, 
     slotd :: <dood-slot-descriptor>, offset :: <integer>,
     force-proxy :: <function>, mark-proxy :: <function>)
  let commit?     = walk-info-commit?(info);
  let flush?      = walk-info-flush?(info);
  let force?      = walk-info-force?(info);
  let force-lazy? = commit? | (~flush? & force?);
  let (value, lazy?)
    = dood-lazy-slot-value-using
        (dood, object, slotd, offset, force?, flush?, commit?,
	 force-proxy, mark-proxy);
  if (force-lazy? | ~(lazy? | flush?))
    walk-slot(dood, info, object, value, queue?: $queue-lazy-walk?);
  end if;
end function;

define method walk-slots (dood :: <dood>, info :: <walk-info>, object) => ()
  let class = object-class(object);
  let (lazy-slotds, weak-slotds, deep-slotds, 
       repeated-slot?, repeated-byte-slot?) 
    = dood-all-slot-descriptors(dood, class);

  walk-slot(dood, info, object, class);
  when (repeated-slot?)
    walk-slot(dood, info, object, size(object));
  end when;
  for (slotd :: <dood-slot-descriptor> in deep-slotds)
    let value = dood-slot-value(object, slotd);
    walk-slot(dood, info, object, value);
  finally
    for (slotd :: <dood-slot-descriptor> in lazy-slotds,
	 // TODO: this is inefficient -- only for lazy-slot-value
	 offset :: <integer> from size(deep-slotds)) 
      walk-lazy-slot
	(dood, info, object, slotd, offset,
	 dood-force-lazy-slot-value-proxy, mark-lazy-slot-using);
    finally
      if (repeated-slot?)
        if (repeated-byte-slot?)
	  if (walk-info-commit?(info))
	    dood-write-string(dood, object)
	  end if
        else
          for (value in object)
            walk-slot(dood, info, object, value);
          end for;
        end if;
      end if;
    end for;
  end for;
end method;

// define function dood-flush-walked-objects (dood :: <dood>) => ()
//   for (i :: <integer> from 0, object in dood-walked-objects(dood))
//     if (i > 0 & modulo(i, 10000) = 0)
//       // format-out("FLUSHED %=\n", i);
//     end if;
//     mark-lazy-slots(dood, object)
//   end for;
// end function;

define inline function dood-walked-address-using-table
    (dood :: <dood>, object) => (res :: false-or(<address>))
  // element(dood-walked-addresses(dood), object, default: #f)
  two-level-table-element
    (#f, dood-walked-addresses(dood), object, object-class, identity, #f);
end function;

define inline function dood-walked-address
    (dood :: <dood>, object) => (res :: false-or(<address>))
  dood-walked-address-using-table(dood, object)
end function;

define inline function dood-do-register-walked-address-using-table
    (dood :: <dood>, object, address)
  // dood-walked-addresses(dood)[object] := address; 
  two-level-table-element
     (#f, dood-walked-addresses(dood), object, object-class, identity) 
    := address;
end function;

define inline function dood-register-walked-address-using-table
    (dood :: <dood>, object, address)
  // dood-walked-addresses(dood)[object] := address; 
  unless (dood-addresses(dood) == dood-walked-addresses(dood))
    dood-do-register-walked-address-using-table(dood, object, address)
  end unless;
end function;

define method dood-register-walked-pointer
    (dood :: <dood>, object, disk-object, pointer :: <pointer>)
  let address = untag(pointer);
  dood-register-walked-address-using-table(dood, object, address);
  address
end method;

define function dood-register-walked-object (dood :: <dood>, object)
  // push-last(dood-walked-objects(dood), object);
end function;

define constant $default-walk-progress-function
  = method (count) format-out("@"); force-output(*standard-output*); end;

define thread variable *walk-progress-function* :: <function>
  = $default-walk-progress-function;

define macro with-walk-progress
  { with-walk-progress (?progress:body) ?:body end }
    => { dynamic-bind (*walk-progress-function* 
                         = method (?=count) ?progress end)
           ?body
         end dynamic-bind }
end macro;

// define variable *table* = make(<table>);

define variable *dump-all-objects?* = #f;

define function walk-object
    (dood :: <dood>, info :: <walk-info>, object, address :: <address>)
 // => (pointer :: <pointer>)
  let count = dood-walked-count(dood);
  dood-walked-count(dood) := count + 1;
  // format-out("%5d: %=\n", count, object-class(object));
  // let entry = element(*table*, object-class(object), default: #f)
  //               | (element(*table*, object-class(object)) := list(0));
  // head(entry) := head(entry) + 1;
  // if (count = 9999)
  //   break("9999");
  // end if;
  // dood-register-walked-object(dood, object);
  when (*dump-all-objects?*
	  & *walk-progress-function* == $default-walk-progress-function)
    block ()
      format-out("%5d %=\n", count, object);
    exception (<error>)
      format-out("\n");
    end block;
  end when;
  if (count > 0 & modulo(count, 10000) = 0)
    *walk-progress-function*(count)
  end if;
  unless (walk-info-commit?(info))
    walk-info-function(info)(object);
  end unless;
  dood-position(dood) := address;
  walk-slots(dood, info, object);
end function;

define function walk-pending-object
    (dood :: <dood>, info :: <walk-info>, object, parent, reference :: false-or(<address>))
  let pointer = maybe-walk-object(dood, info, parent, object, #f);
  // patch pending references
  when (reference)
    with-saved-position (dood)
      dood-write-at(dood, pointer, reference);
    end with-saved-position;
  end when;
end function;

define method dood-reset-walker!
    (dood :: <dood>, #rest all-keys, #key size) => ()
  dood-walked-count(dood) := 0;
  dood-walked-addresses(dood) := apply(make-object->address-table, all-keys);
  // remove-all-keys!(dood-walked-addresses(dood));
  // remove-all-keys!(dood-walked-mark-addresses(dood));
  remove-all-keys!(dood-back-pointers(dood));
//  dood-walked-objects(dood) := make(<stretchy-vector>);
  dood-walk-queue(dood)     := make(<dood-queue>);
end method;

define function dood-clear-walked-objects! (dood :: <dood>) => ()
  dood-initialize-walker!(dood);
end function;

define method dood-register-predefines (dood :: <dood>) => ()
  for (object in dood-predefines(dood))
    dood-register-walked-address-using-table
      (dood, object, dood-address(dood, object));
  end for; 
end method;

define method dood-initialize-walker! 
    (dood :: <dood>, #rest all-keys, #key size) => ()
  apply(dood-reset-walker!, dood, all-keys);
  dood-register-predefines(dood);
end method;

define method dood-finalize-walker! (dood :: <dood>) => ()
  // for (object in dood-walked-objects(dood))
  //   maybe-clear-mark(object);
  // end for;
end method;

define method dood-walk-from
    (dood :: <dood>, fn :: <function>, object, 
     #rest all-keys, #key flush?, force? = #t, parents?, batch?, commit?)
  block ()
    let info = apply(make, <walk-info>, function: fn, all-keys);
    let queue = make(<dood-queue>);
    dood-walk-queue(dood) := queue;
    maybe-walk-object(dood, info, #f, object, $queue-walk?);
    until (dood-queue-empty?(queue))
      let object    = dood-queue-pop(queue);
      let parent    = dood-queue-pop(queue);
      let segment   = dood-queue-pop(queue);
      let reference = commit? & dood-queue-pop(queue);
      with-segment-excursion (dood, segment)
        walk-pending-object(dood, info, object, parent, reference)
      end with-segment-excursion;
    end until;
    // format-out("MAX DOOD QUEUE WIDTH = %=\n", dood-queue-capacity(queue));
    // for (entry keyed-by class in *table*)
    //   format-out("%4d %=\n", head(entry), class);
    // end for;
  cleanup
    dood-finalize-walker!(dood);
  end block;
end method;  

define method dood-walk
    (dood :: <dood>, fn :: <function>, #rest all-keys, #key, #all-keys) => ()
  dood-initialize-walker!(dood);
  apply(dood-walk-from, dood, fn, dood-root(dood), all-keys)
end method;  
  
/// WALK METHODS

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, object :: <machine-word>) => ()
  walk-slot(dood, info, object, <machine-word>);
  if (walk-info-commit?(info))
    dood-write-machine-word(dood, object);
  end if
end method;

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, object :: <single-float>) => ()
  walk-slot(dood, info, object, <single-float>);
  if (walk-info-commit?(info))
    dood-write-machine-word(dood, decode-single-float(object));
  end if;
end method;

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, object :: <double-float>) => ()
  walk-slot(dood, info, object, <double-float>);
  if (walk-info-commit?(info))
    let (low, high) = decode-double-float(object);
    dood-write-machine-word(dood, low);
    dood-write-machine-word(dood, high);
  end if;
end method;

define method dood-flush-from (dood :: <dood>, function :: <function>, object)
  format-out("    FLUSHING %=\n", object);
  dood-initialize-walker!(dood);
  dood-walk-from(dood, function, object, batch?: #f, flush?: #t, force?: #f);
end method;

define method dood-flush-from-if
    (dood :: <dood>, function :: <function>, test :: <function>)
  for (object keyed-by address in dood-objects(dood))
    when (test(object))
      dood-flush-from(dood, function, object);
    end when;
  end for;
end method;

define method dood-flush-all (dood :: <dood>, function :: <function>)
  // dump(dood);
  for (object keyed-by address in dood-objects(dood))
    unless (address < $dood-predefines-end | lookup-proxy(dood, object))
      // format-out("  FLUSHING %= @ %=\n", object, address);
      mark-lazy-slots-at(dood, object, address, force?: #f);
      dood-reinitialize(dood, object);
    end unless;
  end for;
end method;

// eof
