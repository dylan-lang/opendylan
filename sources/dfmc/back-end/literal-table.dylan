module: dfmc-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define variable *literal-heap-stats?* = #f; // = #"verify"

define constant *merge-pairs?* = #t;

define constant *merge-types?* = #t;


define class <literal-table> (<table>) end;
define sealed-constructor <literal-table>;

// Should return true when object has a literal equal test other than ==,
// so we can avoid interning things which we don't merge anyhow.
define generic literal-mergable? (object) => (well?);

define generic literal-equal? (object1, object2) => (well?);

define generic literal-hash
    (object :: <object>, depth :: <integer>, state :: <hash-state>)
  => (id :: <integer>, state :: <hash-state>);

define function literal-hash-test (object, state :: <hash-state>)
  literal-hash(object, 0, state)
end;

define method immutable-literal-equal? (obj1, obj2)
  literal-equal?(obj1, obj2)
end method;

define sealed method table-protocol
    (table :: <literal-table>) => (test :: <function>, hash :: <function>)
  values(immutable-literal-equal?, literal-hash-test)
end method table-protocol;

// define function literal-lookup (table :: <literal-table>, object :: <object>)
//  => (object :: <object>, changed?)
//   if (~literal-mergable?(object))
//     values(object, #f)
//   else
//     let new = element(table, object, default: #f);
//     if (new)
//       values(new, new ~== object)
//     else
//       table[object] := object;
//       values(object, #f)
//     end;
//   end;
// end function;

// Default methods

define method literal-mergable? (object) => (well?)
  #f
end method;

define method literal-equal? (object1, object2) => (well?);
  object1 == object2
end method;

define method literal-hash (object, depth :: <integer>, state :: <hash-state>)
  => (id :: <integer>, state :: <hash-state>)
  ignore(depth);
  object-hash(object, state)
end method;


// <simple-object-vector>

define method literal-mergable? (object :: <simple-object-vector>) => (well?)
  #t
end method;

// Note this is a immutable-literal-equal? method, not a literal-equal? method.
// The latter is used for recursive test, and we can't assume internal
// vectors are mergable just because they are pointed to from a
// mergable object.
define method immutable-literal-equal? (object1 :: <simple-object-vector>,
                                        object2 :: <simple-object-vector>)
 => (well?);
  object-class(object1) == object-class(object2)
    & object1.size == object2.size
    & every?(literal-equal?, object1, object2)
end method;

define method literal-hash (vec :: <simple-object-vector>, depth :: <integer>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  if (depth == 0)
    local method next-hash-code (hash, index, state)
            let (elt-hash, state) = literal-hash(vec[index], depth + 1, state);
            values(merge-hash-ids(hash, elt-hash, ordered: #t), state)
          end method next-hash-code;
    let sz = size(vec);
    select (sz)
      0 =>
        values(21011959, state);
      1 =>
        next-hash-code(1, 0, state);
      2 =>
        let (hash, state) = next-hash-code(2, 0, state);
        next-hash-code(hash, 1, state);
      otherwise =>
        let (hash, state) = next-hash-code(sz, 0, state);
        let (hash, state) = next-hash-code(hash, ash(sz, -1), state);
        next-hash-code(hash, sz - 1, state);
    end select
  else
    object-hash(vec, state)
  end;
end method;

// <pair>

define method literal-mergable? (object :: <pair>) => (well?)
  *merge-pairs?*
end method;


// For now assume if pair is immutable, so are its head and tail.  I think
// this is true for all internal data.  I'm not sure whether it's supposed
// to be true for user literals - but then the obvious workaround is not to
// use literals...
define method immutable-literal-equal? (pair1 :: <pair>, pair2 :: <pair>) => (well?)
  iterate loop (fast1 :: <pair> = pair1, fast2 :: <pair> = pair2,
                slow1 :: <pair> = pair1, slow2 :: <pair> = pair2)
    // Can't catch loops through 'head' here, so just avoid the problem by not
    // recursing through pair's in 'head'.
    let h1 = fast1.head;
    let h2 = fast2.head;
    if (h1 ~== h2
          & (instance?(h1, <pair>) | ~immutable-literal-equal?(h1, h2)))
      #f
    elseif (fast1.tail == fast2.tail) // e.g. both #()
      #t
    elseif (~instance?(fast1.tail, <pair>) | ~instance?(fast2.tail, <pair>))
      immutable-literal-equal?(fast1.tail, fast2.tail);
    elseif (fast1.tail == slow1) // circular
      fast2.tail == slow2
    else
      let fast1 :: <pair> = fast1.tail;
      let fast2 :: <pair> = fast2.tail;
      let h1 = fast1.head;
      let h2 = fast2.head;
      if (h1 ~== h2
            & (instance?(h1, <pair>) | ~immutable-literal-equal?(h1, h2)))
        #f
      elseif (fast1.tail == fast2.tail)
        #t
      elseif (~instance?(fast1.tail, <pair>) | ~instance?(fast2.tail, <pair>))
        immutable-literal-equal?(fast1.tail, fast2.tail)
      else
        loop(fast1.tail, fast2.tail, slow1.tail, slow2.tail);
      end;
    end;
  end;
end;

define method literal-hash (pair :: <pair>, depth :: <integer>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  if (depth == 0)
    let hd = pair.head;
    let (hash, state) = if (instance?(hd, <pair>)) object-hash(hd, state);
                        else literal-hash(hd, 0, state) end;
    let tl = pair.tail;
    if (tl == pair)
      values(hash, state)
    elseif (instance?(tl, <pair>))
      let hd = tl.head;
      let (elt-hash, state)
             = if (instance?(hd, <pair>)) object-hash(hd, state);
               else literal-hash(hd, 0, state) end;
      values(merge-hash-ids(hash, elt-hash, ordered: #t), state);
    else
      let (elt-hash, state) = literal-hash(tl, 0, state);
      values(merge-hash-ids(hash, elt-hash, ordered: #t), state);
    end;
  else
    object-hash(pair, state)
  end;
end;


// <byte-string>

define method literal-mergable? (object :: <byte-string>) => (well?)
  #t
end method;

// Merge all byte strings.  All compiler-generated strings are immutable
// and user strings are all source-literals and hence immutable too.
define method literal-equal? (object1 :: <byte-string>, object2 :: <byte-string>)
 => (well?);
  object1 = object2
end method;

define method literal-hash (str :: <byte-string>, depth :: <integer>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  ignore(depth);
  string-hash(str, state)
end method;

// <&limited-integer>

define method literal-mergable? (type :: <&limited-integer>) => (well?)
  *merge-types?*
end method;

define method literal-equal? (t1 :: <&limited-integer>, t2 :: <&limited-integer>) => (well?)
  t1.^limited-integer-min == t2.^limited-integer-min
  & t1.^limited-integer-max == t2.^limited-integer-max
end method;

define method literal-hash
    (type :: <&limited-integer>, depth :: <integer>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>)
  ignore(depth);
  let (hash1, state) = literal-hash(type.^limited-integer-min, depth + 1, state);
  let (hash2, state) = literal-hash(type.^limited-integer-max, depth + 1, state);
  values(merge-hash-ids(hash1, hash2, ordered: #t), state)
end method;

// <&singleton>

define method literal-mergable? (type :: <&singleton>) => (well?)
  *merge-types?*
end method;

define method literal-equal? (s1 :: <&singleton>, s2 :: <&singleton>) => (well?)
  // TODO: shouldn't the objects have been merged already so need only
  // use ==?
  literal-equal?(s1.^singleton-object, s2.^singleton-object)
end;

define method literal-hash (s :: <&singleton>, depth :: <integer>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>)
  if (depth > 3)
    values(0, state)
  else
    literal-hash(s.^singleton-object, depth + 1, state)
  end;
end;


define method literal-mergable? (type :: <&union>) => (well?)
  *merge-types?*
end method;

define method literal-equal? (t1 :: <&union>, t2 :: <&union>) => (well?)
  // TODO: unions of unions...
  // TODO: shouldn't the unionees have been merged already so need only ==?
  (literal-equal?(t1.^union-type1, t2.^union-type1)
     & literal-equal?(t1.^union-type2, t2.^union-type2)) |
  (literal-equal?(t1.^union-type1, t2.^union-type2)
     & literal-equal?(t1.^union-type2, t2.^union-type1))
end method;

define method literal-hash
    (type :: <&union>, depth :: <integer>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>)
  if (depth > 3)
    values(0, state)
  else
    let (hash1, state) = literal-hash(type.^union-type1, depth + 1, state);
    let (hash2, state) = literal-hash(type.^union-type2, depth + 1, state);
    values(merge-hash-ids(hash1, hash2), state)
  end;
end method;

// <&signature>
define method literal-mergable? (sig :: <&signature>) => (well?)
  #t
end method;

define method literal-equal? (s1 :: <&signature>, s2 :: <&signature>) => (well?)
  local method vec-equal? (v1, v2, n)
          block (return)
            for (i from 0 below n)
              unless (literal-equal?(v1[i], v2[i])) return(#f) end;
            finally #t
            end;
          end
        end;
  object-class(s1) == object-class(s2)
    & s1.^signature-properties == s2.^signature-properties
    & vec-equal?(s1.^signature-required, s2.^signature-required,
                 s1.^signature-number-required)
    & vec-equal?(s1.^signature-values, s2.^signature-values,
                 s1.^signature-number-values)
    & literal-equal?(s1.^signature-rest-value, s2.^signature-rest-value)
    & begin
        let n = s1.^signature-number-keys;
        when (n == s2.^signature-number-keys)
          n == 0 |
            (vec-equal?(s1.^signature-keys, s2.^signature-keys, n) &
             vec-equal?(s1.^signature-key-types, s2.^signature-key-types, n))
        end
      end
end;

define method literal-hash
    (sig :: <&signature>, depth :: <integer>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>)
  let (hash, state) = object-hash(object-class(sig), state);
  let hash = merge-hash-ids(sig.^signature-properties, hash);
  values(hash, state);
end;

