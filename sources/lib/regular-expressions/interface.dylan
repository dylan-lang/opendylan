module:   regular-expressions
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: This provides a useable interface for users. Functions 
	  defined outside this file are really too strange and quirky 
          to be of use to people.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /scm/cvs/fundev/Sources/lib/regular-expressions/interface.dylan,v 1.1 2004/03/12 00:08:52 cgay Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// Functions that aren't exported are marked as such.  Everything else
// is exported.
//
// There are quite a few make-fooer functions hanging around.  Now
// that regexp-position does caching, these are basically useless, but
// we've kept them around for backwards compatibility.  Unfortunately,
// internally most of the functions are implemented in terms of
// make-regexp-positioner.  To minimize the amount of rewriting, I've
// liberally applied seals and inline declarations so that
// make-regexp-positioner won't clobber all type information.  The
// downside, of course, is that everything's sealed, but hey, no one
// ever subclassed regexp-position anyway.


// Caching
//
// Parsing a regexp is not cheap, so we cache the parsed regexps and
// only parse a string if we haven't seen it before.  Because in
// practice almost all regexp strings are string literals, we're free
// to choose == or = depending on whatever's fastest.  However,
// because a string is parsed differently depending on whether the
// search is case sensitive or not, we also have to keep track of that
// information as well.  (The case dependent parse boils down to the
// parse creating a <character-set>, which must be either case
// sensitive or case insensitive)
//
// ### Currently, only regexp-position uses this cache, because the
// other functions are still using make-regexp-positioner.  With
// caching, that make-regexp-whatever stuff should probably go.

// <cache-key> -- internal
//
// What we use for keys in the *regexp-cache*.
//
define class <cache-key> (<object>)
  constant slot regexp-string :: <string>, 
    required-init-keyword: #"regexp-string";
  constant slot character-set-type :: <class>, 
    required-init-keyword: #"character-set-type";
end class <cache-key>;

// <cache-element> -- internal
//
// What we use for elements in a *regexp-cache*
//
define class <cache-element> (<object>)
  constant slot parse-tree :: <parsed-regexp>,
    required-init-keyword: #"parse-tree";
  constant slot last-group :: <integer>,
    required-init-keyword: #"last-group";
end class <cache-element>;

// <regexp-cache> -- internal
//
// Maps <cache-key> to <cache-element>.  ### Ideally, we'd be using
// weak pointers to these strings.  In practice, however, most of the
// regexp strings are literals, so this isn't usually a drawback.
//
// For speed, we compare strings with == rather than = (thus
// object-table).  Again, because in practice we're dealing mostly
// with literals, == and = should be almost identical.
//
define class <regexp-cache> (<table>) end;

// table-protocol{<regexp-cache>} -- method on imported G.F.
//
define method table-protocol (table :: <regexp-cache>) 
 => (equal? :: <function>, hash :: <function>);
  /*
  values(method (key1 :: <cache-key>, key2 :: <cache-key>) // equal?
	  => res :: <boolean>;
	   key1.regexp-string == key2.regexp-string
	     & key1.character-set-type == key2.character-set-type;
	 end method,
	 method (key :: <cache-key>) => (id :: <integer>, state); // hash()
	   let (string-id, string-state) = object-hash(key.regexp-string);
	   let (set-type-id, set-type-state) 
	     = object-hash(key.character-set-type);
	   merge-hash-codes(string-id, string-state, 
			    set-type-id, set-type-state, ordered: #t);
	 end method);
  */
  values(method (key1 :: <cache-key>, key2 :: <cache-key>) // equal?
	  => res :: <boolean>;
	   key1.regexp-string == key2.regexp-string
	     & key1.character-set-type == key2.character-set-type;
	 end method,
	 method (key :: <cache-key>, initial-state) => (id :: <integer>, state); // hash()
	   let (string-id, string-state) = object-hash(key.regexp-string, initial-state);
	   let (set-type-id, set-type-state) 
	     = object-hash(key.character-set-type, string-state);
	   values(merge-hash-ids(string-id, set-type-id, ordered: #t), set-type-state);
	 end method);
end method table-protocol;

// *regexp-cache* -- internal
//
// The only instance of <regexp-cache>.  ### Not threadsafe.
//
define constant *regexp-cache* = make(<regexp-cache>);

// parse-or-use-cached -- internal
//
// Tries to use the cached version of the regexp, and if not possible,
// parses it and adds it to the cache.
//
define inline function parse-or-use-cached 
    (regexp :: <string>, character-set-type :: <class>) 
 => (parsed-regexp :: <parsed-regexp>, last-group :: <integer>);
  let key = make(<cache-key>, regexp-string: regexp, 
		 character-set-type: character-set-type); 
  let (cached?, cached-value) = key-exists?(*regexp-cache*, key);
  if (cached?)
    values(cached-value.parse-tree, cached-value.last-group);
  else
    let (parsed-regexp, last-group) = parse(regexp, character-set-type);
    *regexp-cache*[key] := make(<cache-element>, parse-tree: parsed-regexp,
				last-group: last-group);
    values(parsed-regexp, last-group);
  end if;
end function parse-or-use-cached;

// KJP: added
//
define inline function key-exists? (table :: <table>, key :: <object>)
 => (exists? :: <boolean>, value :: <object>)
  let value = element(table, key, default: unfound());
  if (found?(value))
    values(#t, value)
  else
    values(#f, #f)
  end;
end function;


// Regexp positioner stuff

// Find the position of a regular expression inside a string.  If the
// regexp is not found, return #f, otherwise return a variable number
// of marks.
//
define function regexp-position
    (big :: <string>, regexp :: <string>, #key start: big-start = 0,
     end: big-end = #f, case-sensitive = #f)
 => (regexp-start :: false-or(<integer>), #rest marks :: false-or(<integer>));
  let substring = make(<substring>, string: big, start: big-start,
		       end: big-end | big.size);
  let char-set-class = if (case-sensitive) 
			 <case-sensitive-character-set>;
		       else
			 <case-insensitive-character-set>;
		       end if;
  let (parsed-regexp, last-group) 
    = parse-or-use-cached(regexp, char-set-class);

  let (matched, marks)
    = if (parsed-regexp.is-anchored?)
	anchored-match-root?(parsed-regexp, substring, case-sensitive,
			     last-group + 1, #f);
      else
	let initial = parsed-regexp.initial-substring;
	let searcher = ~initial.empty?
	  & make-substring-positioner(initial, case-sensitive: case-sensitive);
	match-root?(parsed-regexp, substring, case-sensitive, last-group + 1,
		    searcher);
      end if;
  if (matched)  
    apply(values, marks);
  else
    #f  
  end if;
end function regexp-position;

// Once upon a time, this was how you interfaced to the NFA stuff
// (maximum-compile: #t).  That's gone.  Now it's just here for
// backwards compatibility.  All keywords except case-sensitive are
// now ignored.
//
define inline function make-regexp-positioner
    (regexp :: <string>, 
     #key byte-characters-only = #f, need-marks = #t, maximum-compile = #f,
     case-sensitive = #f)
 => regexp-positioner :: <function>;
  method (big :: <string>, #key start: big-start = 0,
	  end: big-end = #f)
   => (regexp-start :: false-or(<integer>), 
       #rest marks :: false-or(<integer>));
    regexp-position(big, regexp, case-sensitive: case-sensitive, 
		    start: big-start, end: big-end);
  end method;
end function make-regexp-positioner;


// #if (have-free-time)
/*
// regexp-matches -- exported
//
// A more convenient form of regexp-position.  Usually you want
// substrings that were matched by a group rather than the marks for
// the group.  How you use this is you give the group numbers you
// want, and it'll give you the strings.  (#f if that group wasn't
// matched)
//
define function regexp-matches
    (big :: <string>, regexp :: <string>,
     #key start: start-index :: <integer> = 0,
          end: end-index :: false-or(<integer>),
          case-sensitive :: <boolean> = #f,
          groups :: false-or(<sequence>))
 => (#rest group-strings :: false-or(<string>));
  if (~groups)
    error("Mandatory keyword groups: not used in call to regexp-matches");
  end if;
  let (#rest marks)
    = regexp-position(big, regexp, start: start-index, end: end-index, 
		      case-sensitive: case-sensitive);
  let return-val = make(<vector>, size: groups.size, fill: #f);
  for (index from 0 below return-val.size)
    let group-start = groups[index] * 2;
    let group-end = group-start + 1;
    if (element(marks, group-start, default: #f))
      return-val[index] := copy-sequence(big, start: 

  let sz = floor/(marks.size, 2);
  let return = make(<vector>, size: sz, fill: #f);
  for (index from 0 below sz)
    let pos = index * 2;
    if (element(marks, pos, default: #f))
      return[index] := copy-sequence(big, start: marks[pos],
				     end: marks[pos + 1]);
    end if;
  end for;
  if (matches)
    let return = make(<vector>, size: matches.size * 2);
    for (raw-pos in matches, index from 0)
      let src-pos = raw-pos * 2;
      let dest-pos = index * 2;
      return[dest-pos] := element(marks, src-pos, default: #f);
      return[dest-pos + 1] := element(marks, src-pos + 1, default: #f);
    end for;
    apply(values, return);
  else
    
    apply(values, marks);
  end if;

// #endif
*/


// Functions based on regexp-position

define function regexp-replace
    (input :: <string>, regexp :: <string>, new-substring :: <string>,
     #key count = #f, case-sensitive = #f, start = 0, end: input-end = #f)
 => changed-string :: <string>;
  let positioner
    = make-regexp-positioner(regexp, case-sensitive: case-sensitive);
  do-replacement(positioner, new-substring, input, start, 
		 input-end, count, #t);
end function regexp-replace;

define inline function make-regexp-replacer 
    (regexp :: <string>, #key replace-with, case-sensitive = #f)
 => replacer :: <function>;
  let positioner
    = make-regexp-positioner(regexp, case-sensitive: case-sensitive);
  if (replace-with)
    method (input :: <string>, #key count: count, 
	    start = 0, end: input-end = #f)
     => string :: <string>;
      do-replacement(positioner, replace-with, input, start, 
		     input-end, count, #t);
    end method;
  else
    method (input :: <string>, new-substring :: <string>, 
	    #key count = #f, start = 0, end: input-end = #f)
     => string :: <string>;
      do-replacement(positioner, new-substring, input, 
		     start, input-end, count, #t);
    end method;
  end if;
end function make-regexp-replacer;

// equivalent of Perl's tr.  Does a character by character translation.
//
define open generic translate
    (input :: <string>, from-set :: <string>, to-set :: <string>, 
     #key delete, start, end: the-end)
 => output :: <string>;

//The existing methods only work on byte-strings.
//
define method translate
    (input :: <byte-string>, from-set :: <byte-string>,
     to-set :: <byte-string>,
     #key delete: delete = #f, start = 0, end: input-end = #f)
 => output :: <byte-string>;
  let table = make-translation-table(from-set, to-set, delete: delete);
  run-translator(input, table, start, input-end | size(input));
end method translate;

define open generic make-translator
    (from-set :: <string>, to-set :: <string>, #key delete)
 => translator :: <function>;

// Again, only byte-strings handled here
//
define method make-translator
    (from-set :: <byte-string>, to-set :: <byte-string>,
     #key delete: delete = #f)
 => translator :: <function>;
  let table = make-translation-table(from-set, to-set, delete: delete);
  method (input :: <byte-string>, #key start = 0, end: input-end = #f)
   => output :: <byte-string>;
    run-translator(input, table, start, input-end | size(input));
  end method;
end method make-translator;

// Used by translate.  Not exported.
//
define function make-translation-table
    (from-set :: <byte-string>, to-set :: <byte-string>,
     #key delete: delete = #f)
 => table :: <byte-character-table>;
  let from-index = 0;
  let to-index = 0;
  let previous-from = #f;
  let previous-to = #f;

     // These local methods are identical except for the 
     // choice of variable names and next-from-character signals end of
     // string rather than repeating the last character indefinitely like
     // next-to-character does.
  local method next-from-character ()
	  if (from-index >= size(from-set))
	    #f;
	  elseif (from-set[from-index] = '\\')
	    from-index := from-index + 2;
	    previous-from := from-set[from-index - 1];
	  elseif (from-set[from-index] = '-')
	    if (previous-from = from-set[from-index + 1])
	      from-index := from-index + 1;
	      from-set[from-index];
	    else
	      previous-from := successor(previous-from); 
	              // and return that value
	    end if;
	  else
	    from-index := from-index + 1;
	    previous-from := from-set[from-index - 1];
	  end if;
	end method next-from-character;

  local method next-to-character ()
	  if (to-index >= size(to-set))
	    if (delete)  #f  else  last(to-set)  end;
	  elseif (to-set[to-index] = '\\')
	    to-index := to-index + 2;
	    previous-to := to-set[to-index - 1];
	  elseif (to-set[to-index] = '-')
	    if (previous-to = to-set[to-index + 1])
	      to-index := to-index + 1;
	      to-set[to-index];
	    else
	      previous-to := successor(previous-to); 
	              // and return that value
	    end if;
	  else
	    to-index := to-index + 1;
	    previous-to := to-set[to-index - 1];
	  end if;
	end method next-to-character;

  let table = make(<byte-character-table>);
  // Wish I had keyed-by
  let (state, limit, next, done?, cur-key, cur-elem)
    = forward-iteration-protocol(table);
  for (st = state then next(table, st), until: done?(table, st, limit))
    let c = cur-key(table, st);
    table[c] := c;
  end for;

  for (from-char = next-from-character() then next-from-character(),
       to-char = next-to-character() then next-to-character(),
       until: from-char = #f)
    table[from-char] := to-char;
  end for;

  table;
end function make-translation-table;

// Used by translate.  Not exported.
//
define function run-translator
    (source :: <byte-string>, table :: <byte-character-table>, 
     start-index :: <integer>, end-index :: <integer>)
 => output :: <byte-string>;
  let dest-string = copy-sequence(source);
  let dest-index = start-index;
  for (source-index from start-index below end-index)
    let char = source[source-index];
    if (table[char] ~== #f)
      dest-string[dest-index] := table[char];
      dest-index := dest-index + 1;
    end if;
  end for;

      // Now resize dest-string, because deleting characters in the
      // translation would make dest-string shorter than we've
      // allocated.
  if (dest-index = end-index)
    dest-string;
  else
    replace-subsequence!(dest-string, "", start: dest-index, end: end-index);
  end if;
end function run-translator;

// Like Perl's split function
//
define function split
    (pattern :: <string>, input :: <string>, 
     #key count = #f, remove-empty-items = #t, start = 0, end: input-end = #f)
 => (#rest whole-bunch-of-strings :: <string>);
  let positioner = make-regexp-positioner(pattern);
  split-string(positioner, input, start, input-end | size(input),
	       count, remove-empty-items);
end function split;

define inline function make-splitter
    (pattern :: <string>) => splitter :: <function>;
  let positioner = make-regexp-positioner(pattern);
  method (string :: <string>, #key count = #f,
	  remove-empty-items = #t, start = 0, end: input-end = #f)
   => (#rest whole-bunch-of-strings :: <string>);
    split-string(positioner, string, start, input-end | size(string), 
		 count, remove-empty-items);
  end method;
end function make-splitter;

// Used by split.  Not exported.
//
define function split-string
    (positioner :: <function>, input :: <string>, start :: <integer>, 
     input-end :: <integer>, count :: false-or(<integer>), 
     remove-empty-items :: <object>)
 => (#rest whole-bunch-of-strings :: <string>);
  let strings = make(<deque>);
  block (done)
    let end-of-last-match = 0;
    let start-of-where-to-look = start;
    let string-number = 1;    // Since count: starts at 1, so 
                              // should string-number
    while (#t)
      let (substring-start, substring-end)
	= positioner(input, start: start-of-where-to-look, end: input-end);
      if (~substring-start | (count & (count <= string-number)))
	push-last(strings, copy-sequence(input, start: end-of-last-match));
	done(); 
      elseif ((substring-start = start-of-where-to-look)
		&  remove-empty-items)
	      // delimited item is empty
	end-of-last-match := substring-end;
	start-of-where-to-look := end-of-last-match;
      else
	let new-string = copy-sequence(input, start: end-of-last-match, 
				       end: substring-start);
	if (~new-string.empty? | ~remove-empty-items)
	  push-last(strings, new-string);
	  string-number := string-number + 1;
	  end-of-last-match := substring-end;
	  start-of-where-to-look := end-of-last-match;
	end if;
      end if;
    end while;
  end block;
  if (remove-empty-items)
    apply(values, remove!(strings, #f, test: method (a, b) a.empty? end));
  else
    apply(values, strings);
  end if;
end function split-string;

// join--like Perl's join
//
// This is not really any more efficient than concatenate-as, but it's
// more convenient.
//
define function join (delimiter :: <byte-string>, #rest strings)
 => big-string :: <byte-string>;
  let length = max(0, (strings.size - 1 ) * delimiter.size);
  for (string in strings)
    length := length + string.size;
  end for;
  let big-string = make(<byte-string>, size: length);
  let big-index = 0;
  for (i from 0 to strings.size - 2)  // Don't iterate over the last string
    let string = strings[i];
    let new-index = big-index + string.size;
    big-string := replace-subsequence!(big-string, string, 
				       start: big-index, end: new-index);
    big-index := new-index;
    let new-index = big-index + delimiter.size;
    big-string := replace-subsequence!(big-string, delimiter, 
				       start: big-index, end: new-index);
    big-index := new-index;
  end for;
  if (strings.size > 0)
    big-string 
      := replace-subsequence!(big-string, strings.last, 
			      start: big-index, end: big-string.size);
  end if;
  big-string;
end function join;
