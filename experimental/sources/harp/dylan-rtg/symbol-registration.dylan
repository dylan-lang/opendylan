module:    dylan-rtg
Synopsis:  A temporary symbol registration mechanism for the Dylan runtime
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant sov-header-size = 8;  // header size for a simple object vector


define dylan-wrapper runtime-external symbol-class = "<symbol>";

// oblist is a static variable which contains a simple object vector of interned symbols
define runtime-variable oblist        = "%oblist", section: #"ambiguous-data";

// oblist-size is the size of that vector in bytes
define runtime-variable oblist-size   = "%oblist_size", data: sov-header-size;

// oblist-cursor is the index into that vector, in bytes, of the first
// unused slot. The initial value is the start of data of the vector.
define runtime-variable oblist-cursor = "%oblist_cursor", data: sov-header-size;


// size in bytes of allocation increments for the oblist
define constant initial-oblist-size = 1024;  



define init runtime-primitive preboot-symbols
  // On entry:
  //    
  // On exit:
  //    A vector of all the symbols which have been interned so far
  //    All symbols registered in this package are flushed.

  nreg data-start, data-size;
  result result;

  // Make a new vector of all symbols interned so far
  ins--add(be, data-start, oblist, sov-header-size);  // start of data to copy
  ins--sub(be, data-size, oblist-cursor, sov-header-size); // size in bytes to copy
  ins--asr(be, data-size, data-size, 2); // size in words;
  let vec = op--make-vector-from-data(be, data-start, data-size);

  // reset the registration variables to flush all data
  ins--move(be, oblist-size, sov-header-size);
  ins--move(be, oblist-cursor, sov-header-size);
  ins--move(be, oblist, 0);

  ins--move(be, result, vec);
  ins--rts-and-drop(be, 0);
end runtime-primitive;



define init runtime-primitive make-symbol
  // On entry:
  //    name  -- the name of the symbol
  // On exit:
  //    A pointer to a freshly alloced symbol

  result result;
  greg name;

  op--load-arguments(be, name); 
  op--make-symbol(be, result, name);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


define init runtime-primitive string-as-symbol
  // On entry:
  //    name  -- the name of the symbol as a <byte-string>
  // On exit:
  //    A pointer to an interned symbol with that name

  result result;
  greg name, symbol;
  tag have-symbol;

  op--load-arguments(be, name);

  // look to see if we already have the symbol
  op--lookup-symbol(be, symbol, name);
  ins--bne(be, have-symbol, symbol, dylan-false);

  // If we don't, then we must make a new one
  op--make-symbol(be, symbol, name);
  op--register-symbol(be, symbol);

  // Now we have the symbol, return it
  ins--tag(be, have-symbol);
  ins--move(be, result, symbol);
  ins--rts-and-drop(be, 0);
end runtime-primitive;



define init runtime-primitive resolve-symbol
  // On entry:
  //    symbol  -- an uninterned symbol
  // On exit:
  //    A pointer to an interned symbol with that name

  greg name, symbol, interned;
  result result;
  tag have-symbol;

  op--load-arguments(be, symbol);
  ins--ld(be, name, symbol, 4);  // get the name

  // look to see if we already have the symbol
  op--lookup-symbol(be, interned, name);
  ins--bne(be, have-symbol, interned, dylan-false);

  // If we don't, then we must intern the one we have
  op--register-symbol(be, symbol);
  ins--move(be, interned, symbol);

  // Now we have an interned symbol, return it
  ins--tag(be, have-symbol);
  ins--move(be, result, interned);
  ins--rts-and-drop(be, 0);
end runtime-primitive;





// DU op to make a fresh symbol
//
define method op--make-symbol
    (be :: <harp-back-end>, symbol :: <register>, name :: <register>)
  op--allocate-traced(be, symbol, 8, symbol-class, name);
end method;


// DU op to look to see if we already have an interned symbol for name
// Sets 'symbol' to #f if it's not interned, or to the symbol if it is.
//
define method op--lookup-symbol
    (be :: <harp-back-end>, symbol :: <register>, name :: <register>)
  with-harp (be)
    tag not-found, loop, all-done;
    greg name2;
    nreg cursor, vec, index;

    // cache the values of the global variables
    ins--move(be, cursor, oblist-cursor);
    ins--move(be, vec, oblist);

    // iterate over the oblist looking for a match
    ins--move(be, index, sov-header-size);
    ins--tag(be, loop);
    ins--bge(be, not-found, index, cursor);
    // look to see if we have the symbol
    ins--ld(be, symbol, vec, index);
    ins--ld(be, name2, symbol, 4);
    op--branch-if-matching(be, all-done, name, name2);
    ins--add(be, index, index, 4);
    ins--bra(be, loop);

    // No symbol was found, so return false
    ins--tag(be, not-found);
    ins--move(be, symbol, dylan-false);
    
    ins--tag(be, all-done);
  end with-harp;
end method;


// TUU op to compare the names of two symbols and branch if they match
define method op--branch-if-matching
    (be :: <harp-back-end>, match :: <tag>, 
     name1 :: <register>, name2 :: <register>)
  with-harp (be)
    tag no-match, match-char, loop;
    nreg index, size1, size2, char1, char2;

    // First load and compare sizes
    ins--ld(be, size1, name1, 4);
    ins--ld(be, size2, name2, 4);
    ins--bne(be, no-match, size1, size2);

    // If the sizes match, then look at each character in turn:-
    op--untaggify(be, index, size1); // size in bytes, untagged
    ins--sub(be, index, index, 1);   // index of the last character
    ins--tag(be, loop);
    ins--blt(be, match, index, 0);   // If we've seen all chars, then we match
    op--load-byte-index(be, char1, name1, index, 8);
    op--load-byte-index(be, char2, name2, index, 8);
    ins--beq(be, match-char, char1, char2); // quick identity test
    // If the characters didn't match, then maybe it's because of their case
    let case-mask = #b100000;
    ins--eor(be, char1, char1, char2);
    ins--bne(be, no-match, char1, case-mask);  // check for case-bit-only difference
    // If the characters are the same apart from the case bit then they match
    // provided that one of them is alphabetic
    ins--or(be, char2, char2, case-mask);     // force alphabetic to lower case
    ins--blt(be, no-match, char2, as(<integer>, 'a'));
    ins--bgt(be, no-match, char2, as(<integer>, 'z'));
    // It's  a match
    // Update the index, and go around the loop again
    ins--tag(be, match-char);
    ins--sub(be, index, index, 1);
    ins--bra(be, loop);

    ins--tag(be, no-match);
  end with-harp;
end method;


// U op to intern an uninterned symbol
//
define method op--register-symbol
    (be :: <harp-back-end>, symbol :: <register>)
  with-harp (be)
    tag oblist-ok;
    c-result c-result;
    greg vec;
    nreg size, old-size, data-size, cursor, vec-data, old-vec-data;

    // cache the values of the global variables
    ins--move(be, size, oblist-size);
    ins--move(be, cursor, oblist-cursor);
    ins--move(be, vec, oblist);

    // check to see whether there is room to intern another symbol
    ins--blt(be, oblist-ok, cursor, size);

    // There isn't enough room in the oblist - so extend it
    ins--sub(be, old-size, size, sov-header-size);
    ins--add(be, size, size, initial-oblist-size);
    ins--add(be, old-vec-data, vec, sov-header-size);
    // make a new oblist & update the global variables
    ins--sub(be, data-size, size, sov-header-size); // size in bytes of the data
    ins--asr(be, data-size, data-size, 2); // size in words
    op--call-c(be, alloc-rf, size, dylan-sov-class, data-size, 1, dylan-unbound);
    ins--move(be, oblist, c-result);
    ins--move(be, vec, c-result);

    ins--add(be, vec-data, c-result, sov-header-size);
    ins--move(be, oblist-size, size);
    // copy the old data into the new vector
    ins--asr(be, old-size, old-size, 2); // old size in words
    ins--copy-words-down-w(be, vec-data, old-vec-data, old-size);

    // We now know that the oblist is big enough, so put in the symbol
    ins--tag(be, oblist-ok);
    ins--st(be, symbol, vec, cursor);
    ins--add(be, cursor, cursor, 4);
    ins--move(be, oblist-cursor, cursor);
  end with-harp;
end method;
