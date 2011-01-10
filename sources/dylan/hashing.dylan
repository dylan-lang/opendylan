Module:    internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic mps-w0
  (o :: <hash-state>) => (w0 :: <raw-machine-word>);
define generic mps-w1
  (o :: <hash-state>) => (w1 :: <raw-machine-word>);

define sealed class <hash-state> (<object>) 
  constant slot mps-w0 :: <raw-machine-word>;
  constant slot mps-w1 :: <raw-machine-word>;
end class <hash-state>;

ignore(mps-w0);
ignore(mps-w1);

define sealed inline method initialize (hs :: <hash-state>, #key)
  next-method();
  primitive-mps-ld-reset(hs)
end method initialize;


define sealed domain make (singleton(<hash-state>));
define sealed domain initialize (<hash-state>);


define inline method merge-hash-state!
    (into :: <hash-state>, hs :: <hash-state>) => (into :: <hash-state>)
  primitive-mps-ld-merge(into, hs); 
  into
end method merge-hash-state!;


define inline method is-stale? (hs :: <hash-state>) => (is-stale? :: <boolean>)
  raw-as-integer(primitive-mps-ld-isstale(hs)) > 0
end method is-stale?;


define function merge-hash-ids (id1 :: <integer>, id2 :: <integer>, #key ordered)
 => (id :: <integer>)
  let id3 = if (ordered)
	      // Left rotate id1 5 bits while being
	      // carefule to avoid <integer> overflow.
	      machine-word-as-hash-index
		(machine-word-unsigned-rotate-left
		   (coerce-integer-to-machine-word(id1), 5))
	    else 
	      id1
	    end if;
  logxor(id2, id3)
end function merge-hash-ids;

// define function merge-hash-states (state1 :: <hash-state>, state2 :: <hash-state>)
//  => (state :: <hash-state>)
//   let hs :: <hash-state> = make(<hash-state>);
//   merge-hash-state!(hs, state1); merge-hash-state!(hs, state2);     
//   hs
// end function merge-hash-states;

// define function merge-hash-codes (id1 :: <integer>, state1 :: <hash-state>,
// 				  id2 :: <integer>, state2 :: <hash-state>,
// 				  #key ordered = #f)
//  => (id :: <integer>, state :: <hash-state>)
//   values(merge-hash-ids(id1, id2, ordered: ordered), 
// 	 merge-hash-states(state1, state2))
// end function merge-hash-codes;

//
// OBJECT-HASH
//

define generic object-hash 
    (object :: <object>, hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>);

//define constant $default-hash = 21011959;

define method object-hash 
    (object :: <object>, hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  primitive-mps-ld-add(hash-state, object);
  values(machine-word-as-hash-index(address-of(object)), hash-state)
end method object-hash;

define method object-hash (object :: <integer>, 
			   hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  values(object, hash-state)
end method object-hash;

define method object-hash (object :: <boolean>,
			   hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  values
    (if (object)
       144223
     else
       191999
     end if,
     hash-state)
end method object-hash;

define method object-hash (object :: <character>,
			   hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  values(as(<integer>, object) + 232333, hash-state)
end method object-hash;

define method object-hash (object :: <machine-word>, hash-state :: <hash-state>)
 => (hi :: <integer>, hash-state :: <hash-state>)
  values(machine-word-as-hash-index(object), hash-state)
end method object-hash;

define method object-hash (object :: <double-integer>, hash-state :: <hash-state>)
 => (hi :: <integer>, hash-state :: <hash-state>)
  let hash-low = machine-word-as-hash-index(%double-integer-low(object));
  let hash-high = machine-word-as-hash-index(%double-integer-high(object));
  values(merge-hash-ids(hash-low, hash-high, ordered: #t), hash-state)
end method object-hash;

///---*** Is this still just an approximation of the hash state or is it correct???!!!
define method object-hash (object :: <single-float>, hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  object-hash(machine-word-as-hash-index(decode-single-float(object)), hash-state)
end method object-hash;

define function string-hash
    (collection :: <string>, hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  let len = collection.size;
  if (len <= 30)
    if (instance?(collection, <byte-string>))
      // copy-down for efficiency.
      let collection :: <byte-string> = collection;
      for (c :: <byte-character> in collection,
	   hash :: <integer> = len
	     then modulo(ash(hash, 6) + as(<integer>, c), 970747))
      finally
	values(hash, hash-state)
      end
    else
      for (c :: <character> in collection,
	   hash :: <integer> = len
	     then modulo(ash(hash, 6) + as(<integer>, c), 970747))
      finally
	values(hash, hash-state)
      end
    end;
  else
    local method next-hash (hash, index)
	    let c :: <character> = collection[index];
	    merge-hash-ids(hash, as(<integer>, c) + 232333, ordered: #t)
	  end method next-hash;
    values(next-hash(next-hash(next-hash(len, 0), ash(len, -1)), len - 1),
	   hash-state)
  end
end function;

/*
define function byte-string-hash
    (collection :: <byte-string>, hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  let h :: <integer> = 0;
  for (c :: <byte-character> in collection,
       hash = 0 then modulo(ash(hash, 6) + as(<integer>, c), 970747))
  finally
    values(hash, hash-state)
  end
end;
*/
/*
define function case-insensitive-string-hash
    (collection :: <byte-string>, hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  local method next-hash (hash, index)
    merge-hash-ids(hash, object-hash(as-lowercase(collection[index]), hash-state), 
                   ordered: #t)
  end method next-hash;
  common-string-hash(collection, next-hash, hash-state)
end function;
*/

define function case-insensitive-string-hash
    (str :: <byte-string>, hash-state :: <hash-state>) 
 => (hash :: <integer>, hash-state :: <hash-state>)
  values(case-insensitive-string-hash-2(str, 0, str.size),
	 hash-state);
end;

define inline method case-insensitive-string-hash-2
    (str :: <byte-string>, s :: <integer>, e :: <integer>) => (h :: <integer>)
  for (i :: <integer> from s below e,
       hash :: <integer> = 0 then
         modulo(ash(hash, 6) + logand(as(<integer>, str[i]), #x9F), 970747))
  finally
    hash
  end;
end case-insensitive-string-hash-2;

define method case-insensitive-string-hash-2
    (str :: <simple-byte-vector>, s :: <integer>, e :: <integer>) => (h :: <integer>)
  for (i :: <integer> from s below e,
       hash :: <integer> = 0 then
	 modulo(ash(hash, 6) + logand(str[i], #x9F), 970747))
  finally
    hash
  end;
end case-insensitive-string-hash-2;

// You can't write a more specific method on collections because 
// any two collections with identical key/element pairs are equal. 
// Because of this, you can't merge-hash-codes with ordered: #t, or
// really anything else interesting. In partial compensation, this
// method hashes the keys as well as the elements. (As long as you
// always put the element before the key when you merge hash codes,
// you *can* use ordered: #t for merging them)

define function collection-hash
    (key-hash :: <function>, element-hash :: <function>, col :: <collection>, 
     hash-state :: <hash-state>, #key ordered :: <boolean> = #f)
 => (id :: <integer>, state :: <hash-state>)
  let current-id = 0;
  for (elt keyed-by key in col)
    let elt-id = element-hash(elt, hash-state);
    let key-id = key-hash(key, hash-state);
    let captured-id1 = merge-hash-ids(elt-id, key-id, ordered: #t);
    current-id := merge-hash-ids(current-id, captured-id1, ordered: ordered);
  end for;
  values(current-id, hash-state);
end function collection-hash;


// This is similar to a collection-hash, except that it hashes things with
// ordered: #t and ignores the sequence keys. USE WITH CAUTION: This
// isn't a proper equal-hash because two collections of different types
// but identical key/element pairs won't generate the same hash id,
// even though the two collections are =.

define function sequence-hash
    (element-hash :: <function>, seq :: <sequence>, 
     hash-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>)
  let current-id = 0;
  for (elt in seq)
    let id = element-hash(elt, hash-state);
    current-id := merge-hash-ids(current-id, id, ordered: #t);
  end for;
  values(current-id, hash-state);
end function sequence-hash;


define function values-hash
    (element-hash :: <function>, hash-state :: <hash-state>, #rest values)
 => (id :: <integer>, state :: <hash-state>)
  sequence-hash(element-hash, values, hash-state)
end;




