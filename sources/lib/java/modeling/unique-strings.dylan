Module: java-modeling
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

format-out ("initing unique-strings.dylan\n") ;

// uniquefying of strings, this is done on a per library basic,
// the *unique-strings-table* being flushed during link phase,
// but the *global-unique-strings-table* holds on to common strings
// captured at back-end load-time, so not constantly re-hashed for
// each compilation record

// must ensure no unique strings hang around elsewhere,
// otherwise the next library compile will pick up incorrect unique string
// ids.

// perhaps we shouldn't reset *unique-strings-count* ?

// currently using two-tier approach to the constants in the code

define sealed class <unique-string> (<object>)
  constant slot the-string :: <byte-string>, required-init-keyword: the-string: ;
  constant slot unique-tag :: <integer>,     required-init-keyword: unique-tag: ;
end;

define method print-object (uniq :: <unique-string>, stream :: <stream>) => ()
  if (valid-unique? (uniq))
    format (stream, "#U%d\"%s\"", uniq.unique-tag, uniq.the-string)
  else
    format (stream, "#XU%d\"%s\"", uniq.unique-tag, uniq.the-string)
  end
end;

// this picks up all the constant strings in the code
define variable *global-unique-strings-table* :: <object-table> = make (<object-table>) ;

// this is the working copy, at init time points to global one
define variable *unique-strings-table* :: <object-table> = *global-unique-strings-table* ;
define variable *unique-strings-count* :: <integer> = 0 ;

define function start-with-unique-strings ()
 format-out ("START UNIQUE STRINGS\n") ;
//  if (*unique-strings-table* ~== *global-unique-strings-table*)
//    finish-with-unique-strings ()
//  end;
//  *unique-strings-table* := make (<object-table>)
end;

define function finish-with-unique-strings () => ()
 format-out ("FINISH UNIQUE STRINGS\n") ;
  let  glob = *global-unique-strings-table* ;
  if (*unique-strings-table* ~== glob)
//// this is broken, because not nested scope - never throw away now
//    clobber-scoped-strings (*unique-strings-table*) ;
//    *unique-strings-table* := glob ;
//    *unique-strings-count* := glob.size
  end;
end;

// not used?
/*
define function clobber-scoped-strings (table)
  for (key in table.key-sequence)
    let  list = table [key] ;
    for (uniq :: <unique-string> in list)
      uniq.unique-tag := -1
    end
  end
end;
*/




define function uniq (str :: <byte-string>) => (uniq :: <unique-string>)
  // first stab at hash function
  // probably don't need to look at every byte
  let  hash = 39827 ;
  for (char in str)
    let  code :: <integer> = as (<integer>, char) ;
    hash := hash * 37 + code ;
    hash := logxor (logand (hash, #xfffff), ash (hash, -20)) ;
  end;
  // look up in table for a match
  block (return)
    let  tab = *unique-strings-table* ;
    let  strings :: <list> = element (tab, hash, default: #()) ;
    for (uniq :: <unique-string> in strings)
      if (str = uniq.the-string) return (uniq) end
    end;
    // no match, check the global table too
    let globtab = *global-unique-strings-table* ;
    if (tab ~== globtab)
      let glob-strings :: <list> = element (globtab, hash, default: #()) ;
      for (uniq :: <unique-string> in glob-strings)
        if (str = uniq.the-string) return (uniq) end
      end
    end;

    // no match, create one - note we assume <byte-string> args are immutable
    // and hence shareable
    let  count = *unique-strings-count* ;
    let  new-uniq = make (<unique-string>, 
			  the-string: str,
			  unique-tag: count);
    tab [hash] := pair (new-uniq, strings) ;
    *unique-strings-count* := count + 1 ;
    new-uniq
  end
end;

define sealed generic ensure-uniq (thing) => (uniq :: <unique-string>) ;

define method ensure-uniq (thing :: <byte-string>) => (uniq :: <unique-string>)
  thing.uniq
end;

define method ensure-uniq (thing :: <unique-string>) => (uniq :: <unique-string>)
  thing
end;


define function valid-unique? (uniq :: <unique-string>) => (unique? :: <boolean>)
  let  str = uniq.the-string ;
  let  hash = 39827 ;
  for (char in str)
    let  code :: <integer> = as (<integer>, char) ;
    hash := hash * 37 + code ;
    hash := logxor (logand (hash, #xfffff), ash (hash, -20)) ;
  end;
  block (return)
    let  tab = *unique-strings-table* ;
    let  strings :: <list> = element (tab, hash, default: #()) ;
    for (uniq :: <unique-string> in strings)
      if (str = uniq.the-string) return (#t) end
    end;
    // no match, check the global table too
    let globtab = *global-unique-strings-table* ;
    if (tab ~== globtab)
      let glob-strings :: <list> = element (globtab, hash, default: #()) ;
      for (uniq :: <unique-string> in glob-strings)
        if (str = uniq.the-string) return (#t) end
      end
    end;
    #f
  end
end;

// not used?!
define function print-unique-strings ()
  for (el in *unique-strings-table*.key-sequence)
    let  strs = *unique-strings-table*[el] ;
    for (str in strs)
      format-out ("%s\n", str)
    end
  end;
  if (*unique-strings-table* ~== *global-unique-strings-table*)
    format-out ("\n") ;
    for (el in *global-unique-strings-table*.key-sequence)
      let  strs = *global-unique-strings-table*[el] ;
      for (str in strs)
        format-out ("%s\n", str)
      end
    end
  end
end;

format-out ("inited unique-strings.dylan\n") ;
