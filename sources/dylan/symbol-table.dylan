Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <symbol-table>(<table>)
end;

define sealed method table-protocol (table :: <symbol-table>)
  => (test :: <function>, hash :: <function>);
  // ignore(table);
  values(case-insensitive-string-equal, case-insensitive-string-hash);
end method table-protocol;

define function case-insensitive-string-equal
    (string1 :: <byte-string>, string2 :: <byte-string>)
  case-insensitive-string-equal-2(string1,
				  string2,
				  0,
				  string2.size)
end case-insensitive-string-equal;

define inline method case-insensitive-string-equal-2
    (string1 :: <byte-string>,
     string2 :: <byte-string>, s2 :: <integer>, e2 :: <integer>)
  when (string1.size == e2 - s2)
    iterate loop (i :: <integer> = s2)
      (i == e2) | begin
		    let c1 :: <byte-character> = string1[i - s2];
		    let c2 :: <byte-character> = string2[i];
		    (c1 == c2 | as-lowercase(c1) == as-lowercase(c2))
		      & loop(i + 1)
		  end
    end;
  end;
end case-insensitive-string-equal-2;

define method case-insensitive-string-equal-2
    (string1 :: <byte-string>,
     string2 :: <simple-byte-vector>, s2 :: <integer>, e2 :: <integer>)
  when (string1.size == e2 - s2)
    iterate loop (i :: <integer> = s2)
      (i == e2) | begin
		    let c1 :: <byte-character> = string1[i - s2];
		    let c2 :: <byte-character> = as(<byte-character>, string2[i]);
		    (c1 == c2 | as-lowercase(c1) == as-lowercase(c2))
		      & loop(i + 1)
		  end
    end;
  end;
end case-insensitive-string-equal-2;

define variable *initial-symbol-table-size* = 5 * 1024;

define variable *symbols-booted?* :: <boolean> = #f;

define constant *symbols* :: <symbol-table> 
  = make(<symbol-table>, weak: #"value", size: *initial-symbol-table-size*);

// TODO: Belongs with tables when more general purpose.

define sealed method gethash-or-set (table :: <symbol-table>, key :: <byte-string>, new-value)
    => new-or-old-value;
  let len :: <integer> = key.size;
  let tv = table-vector(table);
  let token = rehash-token(tv);
  // Ensure that the above occurs before computing the hash code.
  sequence-point();
  // We are relying on the fact that the hash-state is unimportant for
  // string-tables, even when we may be adding entries.
  let id = case-insensitive-string-hash-2(key, 0, len);
  let (index, fkey) = do-search(fkey in (tv, id))
	               let fkey :: <byte-string> = fkey;
                       case-insensitive-string-equal-2(fkey, key, 0, len)
                      end;
  let vals = entry-values(tv);
  let (success?, value) =
    if (~pointer-id?(fkey, $table-entry-empty))
      sequence-point();
      let value = entry-value(vals, index);
      // Force value fetch to occur before token validation.  Allowing value
      // fetch to occur later could lead to inconsistencies due to in place
      // rehash, because a rehash could then clobber the value.
      sequence-point();
      values (rehash-token-valid?(tv, token) & ~table-entry-deleted?(value),
              value)
    else
      values(try-to-puthash-new
               (tv, token, hash-state(tv), index, key, new-value),
             new-value)
    end;

  if (success?)    
    value
  else
    // Store failed for some reason.  Rehash if needed and retry.
    if (needs-rehash?(tv, token))
      rehash-table(table, tv, full?(tv));
    elseif (full?(tv))
      rehash-table(table, tv, #t);
    end;
    gethash-or-set(table, key, new-value); // try again
  end if;
end method gethash-or-set;

define not-inline function %resolve-symbol (x :: <symbol>) => (y :: <symbol>)
  if (*symbols-booted?*)
    let name = symbol-name(x);
    gethash-or-set(*symbols*, name, x);
  else
    primitive-resolve-symbol(x);
  end;
end function;

define function %install-boot-symbols () => ()
  *symbols-booted?* := #t;
  for (x :: <symbol> in primitive-preboot-symbols())
    *symbols*[symbol-name(x)] := x;
  end for;
end function;

define sealed method make
    (class == <symbol>, #key name :: <byte-string>) => (object :: <symbol>)
  if (*symbols-booted?*)
    make-symbol(name)
  else
    primitive-string-as-symbol(name)
  end if
end method;

// Alternate interface, doesn't cons unless needed.
define method make-symbol (str :: <sequence>,
			     #key start: s :: <integer> = 0,
			          end: e :: <integer> = str.size)
 => (sym :: <symbol>)
  let table = *symbols*;
  iterate gethash (first-attempt? = #t)
    let tv = table-vector(table);
    let token = rehash-token(tv);
    // Ensure token fetched before computing hash code.
    sequence-point();
    // Don't need hash state here.
    let id = case-insensitive-string-hash-2(str, s, e);
    let (index, fkey)
      = do-search(fkey in (tv, id))
	  let fkey :: <byte-string> = fkey;
          case-insensitive-string-equal-2(fkey, str, s, e);
        end;
    // Fetch value vector early to allow better scheduling of the loads.
    let vals = entry-values(tv);
    if (~pointer-id?(fkey, $table-entry-empty))
      // Ensure that value is looked up after search is completed.
      sequence-point();
      let value = entry-value(vals, index);
      // Force value fetch to occur before token validation.  Allowing value
      // fetch to occur later could lead to inconsistencies due to in place
      // rehash, because a rehash could then clobber the value.
      sequence-point();
      if (rehash-token-valid?(tv, token) & ~table-entry-deleted?(value))
	value;
      else
	// Rehash has been initiated.
	//      rehash-table(table, tv, #f);      // Why do this?
	with-table-vector-locked (tv) end;  // Just wait on lock instead.
	gethash(#f); // try again
      end if;
    elseif (needs-rehash?(tv, token))
      // TODO: If this is not the first attempt at rehashing then perhaps we 
      // should look for the key during the rehash.
      rehash-table(table, tv, #f);
      gethash(#f);	// try again
    else
      let name = copy-byte-string(str, s, e);
      let value = system-allocate-simple-instance(<symbol>, fill: name);
      if (try-to-puthash-new(tv, token, hash-state(tv), index, name, value))
	value
      else // failed for some reason, punt.
	gethash-or-set (table, name, value)
      end;
    end if;
  end iterate;
end make-symbol;

define sealed copy-down-method make-symbol
    (str :: <byte-string>, #key start: s :: <integer>, end: e :: <integer>)
 => (sym :: <symbol>);

define sealed copy-down-method make-symbol
    (str :: <simple-byte-vector>, #key start: s :: <integer> = 0,
	                               end: e :: <integer> = str.size)
 => (sym :: <symbol>);


define sealed method copy-byte-string
    (src :: <byte-string>, s :: <integer>, e :: <integer>)
  => (str :: <byte-string>)
  let len :: <integer> = e - s;
  let str :: <byte-string> = make(<byte-string>, size: len);
  primitive-replace-bytes!
    (str, primitive-repeated-slot-offset(str), integer-as-raw(0),
     src, primitive-repeated-slot-offset(src), integer-as-raw(s),
     integer-as-raw(len));
  str
end method copy-byte-string;

define sealed method copy-byte-string
    (src :: <simple-byte-vector>, s :: <integer>, e :: <integer>)
  => (str :: <byte-string>)
  let len :: <integer> = e - s;
  let str :: <byte-string> = make(<byte-string>, size: len);
  primitive-replace-bytes!
    (str, primitive-repeated-slot-offset(str), integer-as-raw(0),
     src, primitive-repeated-slot-offset(src), integer-as-raw(s),
     integer-as-raw(len));
  str
end method copy-byte-string;
