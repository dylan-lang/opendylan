module: regular-expressions
author: Nick Kramer (nkramer@cs.cmu.edu)
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /scm/cvs/fundev/Sources/lib/regular-expressions/parse.dylan,v 1.1 2004/03/12 00:08:52 cgay Exp $

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

// This is a program to parse regular expressions. The grammar I'm using is:
//
//      <regexp> ::= <alternative> | <alternative>|<regexp>
//
//      <alternative> ::= <quantified-atom> | <quantified-atom><alternative>
//
//      <quantified-atom> ::= <atom> | <atom><quantifier>
//
//      <quantifier> ::= * | + | ? | {n} | {n,} | {n, m}
//            (where n and m are decimal integers)
//
//      <atom> ::= (<regexp>) | <extended-character>
//
// See "Programming perl", p. 103-104 for more details.
//
// Because an assertion is a type of <extended-character>, this will
// parse a "quantified assertion", which really isn't a legal regular
// expression component.  Match.dylan could go into an infinite loop
// if given this.

define abstract class <parsed-regexp> (<object>)
end class <parsed-regexp>;

define class <mark> (<parsed-regexp>)
  slot child :: <parsed-regexp>,  required-init-keyword: #"child";
  constant slot group-number :: <integer>, required-init-keyword: #"group";
end class <mark>;

define class <union> (<parsed-regexp>)          //    |
  slot left  :: <parsed-regexp>, required-init-keyword: #"left";
  slot right :: <parsed-regexp>, required-init-keyword: #"right";
end class <union>;

define class <alternative> (<parsed-regexp>)    // concatenation
  slot left :: <parsed-regexp>,  required-init-keyword: #"left";
  slot right :: <parsed-regexp>, required-init-keyword: #"right";
end class <alternative>;

define class <parsed-assertion> (<parsed-regexp>)
  constant slot asserts :: <symbol>, required-init-keyword: #"assertion";
end class <parsed-assertion>;

define class <quantified-atom> (<parsed-regexp>)
  slot atom :: <parsed-regexp>, required-init-keyword: #"atom";
  constant slot min-matches :: <integer>, init-value: 0,
    init-keyword: #"min";
  constant slot max-matches :: false-or(<integer>), init-value: #f, 
    init-keyword: #"max";
end class <quantified-atom>;

define abstract class <parsed-atom> (<parsed-regexp>)
end class <parsed-atom>;

define class <parsed-character> (<parsed-atom>)
  constant slot character :: <character>, required-init-keyword: #"character";
end class <parsed-character>;

define class <parsed-string> (<parsed-atom>)
  constant slot string :: <string>, required-init-keyword: #"string";
end class <parsed-string>;

define class <parsed-set> (<parsed-atom>)
  constant slot char-set :: <character-set>, required-init-keyword: #"set";
end class <parsed-set>;

define class <parsed-backreference> (<parsed-atom>)
  constant slot group-number :: <integer>, required-init-keyword: #"group"; 
end class <parsed-backreference>;

// <parse-info> contains some information about the current regexp
// being parsed.  Using a structure is slightly nicer than having
// global variables..
//
define class <parse-info> (<object>)
  slot backreference-used :: <boolean>, init-value: #f;
     // Whether or not the function includes \1, \2, etc in the regexp.
     // This is different from return-marks, which determines whether the
     // user wants to know about the marks.
  slot has-alternatives :: <boolean>, init-value: #f;
  slot has-quantifiers :: <boolean>, init-value: #f;
  slot current-group-number :: <integer>, init-value: 0;
  constant slot set-type :: <class>, required-init-keyword: #"set-type";
end class <parse-info>;

define class <illegal-regexp> (<error>)
  constant slot regular-expression :: <string>, 
    required-init-keyword: #"regexp";
end class <illegal-regexp>;

define sealed domain make (singleton(<illegal-regexp>));
define sealed domain initialize (<illegal-regexp>);

/* KJP: Doesn't work this way in Functional Developer.
define sealed method report-condition (cond :: <illegal-regexp>, stream) => ();
  condition-format(stream, "Illegal regular expression: \n"
		     "A sub-regexp that matches the empty string has"
		     " been quantified in\n   %s",
		   cond.regular-expression);
end method report-condition;
*/
ignorable(regular-expression);

define method parse (regexp :: <string>, character-set-type :: <class>)
 => (parsed-regexp :: <parsed-regexp>, last-group :: <integer>,
     backrefs? :: <boolean>, alternatives? :: <boolean>, 
     quantifiers? :: <boolean>);
  let parse-info = make(<parse-info>, set-type: character-set-type);
  let parse-string = make(<parse-string>, string: regexp);
  let parse-tree = make(<mark>, group: 0, 
			child: parse-regexp(parse-string, parse-info));
  let optimized-regexp = optimize(parse-tree);
  if (optimized-regexp.pathological?)
    signal(make(<illegal-regexp>, regexp: regexp));
  else
    values(optimized-regexp,
	   parse-info.current-group-number,
	   parse-info.backreference-used,
	   parse-info.has-alternatives,
	   parse-info.has-quantifiers);
  end if;
end method parse;

define method parse-regexp (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let alternative = parse-alternative(s, info);
  if (lookahead(s) = '|')
    info.has-alternatives := #t;
    make(<union>, left: alternative, right: parse-regexp(consume(s), info));
  else
    alternative;
  end if;
end method parse-regexp;

define method parse-alternative (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let term = parse-quantified-atom(s, info);
  if (member?(lookahead(s), #(#f, '|', ')')))
    term;
  else
    make(<alternative>, left: term, right: parse-alternative(s, info));
  end if;
end method parse-alternative;

define method parse-quantified-atom (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let atom = parse-atom(s, info);
  let char = lookahead(s);
  select (char by \=)
    '*' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 0, atom: atom);

    '+' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 1, atom: atom);

    '?' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 0, max: 1, atom: atom);

    '{' =>
      info.has-quantifiers := #t;
      consume(s);
      let first-string = make(<deque>);
      let second-string = make(<deque>);
      let has-comma = #f;
      for (c = lookahead(s) then lookahead(s), until: c = '}')
	consume(s);
	if (c = ',')  
	  has-comma := #t;
	elseif (has-comma)  
	  push-last(second-string, c);
	else 
	  push-last(first-string, c);
	end if;
      end for;
      consume(s);         // Eat closing brace
      make(<quantified-atom>, atom: atom, 
	   min: sequence-to-integer(first-string), // KJP: string-to -> sequence-to
	   max:  if (~has-comma)    
		   sequence-to-integer(first-string)
		 elseif (empty?(second-string))   
		   #f
		 else
		   sequence-to-integer(second-string) 
		 end if);

    otherwise =>
      atom;
  end select;
end method parse-quantified-atom;

// KJP: added, quickie
//
define method sequence-to-integer (seq :: <deque>) => (int :: <integer>)
  string-to-integer(as(<byte-string>, seq));
end method sequence-to-integer;

define method parse-atom (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let char = lookahead(s);
  select (char)
    '(' =>
      consume(s);   // Consume beginning paren
      info.current-group-number := info.current-group-number + 1;
      let this-group = info.current-group-number;
      let regexp = parse-regexp(s, info);
      if (lookahead(s) ~= ')')
	error("Unbalanced parens in regexp");
      end if;
      consume(s);   // Consume end paren
      make(<mark>, child: regexp, group: this-group);

    ')' =>
      #f;             // Need something to terminate upon seeing a close paren

    #f  =>
      #f;   // Signal error?  (end of stream)

    '*', '|', '+' =>
      #f;

    '\\' =>
      consume(s);        // Consume the backslash
      parse-escaped-character(s, info);

    '[' =>
      consume(s);        // Eat the opening brace
      let set-string = make(<deque>);      // Need something that'll 
                                           // preserve the right ordering
      for (char = lookahead(s) then lookahead(s), until: char == ']')
	consume(s);                    // eat char
	if (char ~== '\\')
	  push-last(set-string, char);
	else
	  let char2 = lookahead(s);
	  consume(s);  // Eat escaped char
	  if (char2 == ']')
	    push-last(set-string, ']');
	  else
	    push-last(set-string, '\\');
	    push-last(set-string, char2);
	  end if;
	end if;
      end for;
      consume(s);     // Eat ending brace
      make(<parsed-set>, set: make(info.set-type, description: set-string));

    '.' =>
      consume(s);
      dot;

    '^' =>
      consume(s);
      make(<parsed-assertion>, assertion: #"beginning-of-string");

    '$' =>
      consume(s);
      make(<parsed-assertion>, assertion: #"end-of-string");
  
      // Insert more special characters here

    otherwise =>
      let char = lookahead(s);
      consume(s);
      make(<parsed-character>, character: char);
  end select;
end method parse-atom;

define constant any-char 
  = make(<case-sensitive-character-set>, description: "^\n");

// The useful definitions of all these is in as(<character-set>)
//
define constant digit-chars
  = make(<case-sensitive-character-set>, description: "\\d");
define constant not-digit-chars
  = make(<case-sensitive-character-set>, description: "^\\d");
define constant word-chars
  = make(<case-sensitive-character-set>, description: "\\w");
define constant not-word-chars
  = make(<case-sensitive-character-set>, description: "^\\w");
define constant whitespace-chars
  = make(<case-sensitive-character-set>, description: "\\s");
define constant not-whitespace-chars
  = make(<case-sensitive-character-set>, description: "^\\s");

define constant dot = make(<parsed-set>, set: any-char);
/* KJP: Not used.
define constant dot-star = make(<quantified-atom>, min: 0, max: #f,
				atom: dot);
*/

// This only handles escaped characters *outside* of a character
// set. Inside of a character set is a whole different story.
//
define method parse-escaped-character 
    (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let next-char = lookahead(s);
  consume(s);
  select (next-char)
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' =>
      info.backreference-used := #t;
      make(<parsed-backreference>, group: digit-to-integer(next-char));

    'n' =>   make(<parsed-character>, character: '\n');   // Newline
    't' =>   make(<parsed-character>, character: '\t');   // Tab
    'f' =>   make(<parsed-character>, character: '\f');   // Formfeed
    'r' =>   make(<parsed-character>, character: '\r');   // Carriage return

    'b' =>   make(<parsed-assertion>, assertion: #"word-boundary");
    'B' =>   make(<parsed-assertion>, assertion: #"not-word-boundary");
       // Beginning and end of string are not escaped

    'd' =>   make(<parsed-set>, set: digit-chars);
    'D' =>   make(<parsed-set>, set: not-digit-chars);
    'w' =>   make(<parsed-set>, set: word-chars);
    'W' =>   make(<parsed-set>, set: not-word-chars);
    's' =>   make(<parsed-set>, set: whitespace-chars);
    'S' =>   make(<parsed-set>, set: not-whitespace-chars);

    // Insert more escaped characters here

    otherwise =>
      make(<parsed-character>, character: next-char);
  end select;
end method parse-escaped-character;

define method is-anchored? (regexp :: <parsed-regexp>)
 => (result :: <boolean>);
  select (regexp by instance?)
    <mark> => is-anchored?(regexp.child);
    <alternative> => is-anchored?(regexp.left);
    <parsed-assertion> => regexp.asserts == #"beginning-of-string";
    otherwise => #f;
  end select;
end method is-anchored?;

define method initial-substring (regexp :: <parsed-regexp>)
 => (result :: <string>);
  let result = make(<deque>);
  local method init (regexp :: <parsed-regexp>, result :: <deque>)
	  select (regexp by instance?)
	    <alternative> =>
	      init(regexp.left, result) & init(regexp.right, result);
	    <parsed-character> =>
	      push-last(result, regexp.character);
	    <parsed-string> =>
	      for (ch in regexp.string) push-last(result, ch) end for;
	    <mark> =>
	      init(regexp.child, result);
	    <parsed-assertion> =>
	      #t;
	    otherwise =>
	      #f;
	  end select;
	end method init;
  init(regexp, result);
  as(<byte-string>, result);
end method initial-substring;

// Optimize converts a parse tree into an "optimized" parse tree.
// Currently the only optimization is merging adjacent characters into
// a string.
//
define method optimize (regexp :: <parsed-regexp>)
 => (regexp :: <parsed-regexp>);
  select (regexp by instance?)
    <mark> =>
      regexp.child := optimize(regexp.child);
      regexp;
    <alternative> =>
      if (instance?(regexp.left, <parsed-character>))
	let result-str = make(<deque>);
	push-last(result-str, regexp.left.character);
	for (next = regexp.right then next.right,
	     while: (instance?(next, <alternative>)
		       & instance?(next.left, <parsed-character>)))
	  push-last(result-str, next.left.character)
	finally
	  if (instance?(next, <parsed-character>))
	    push-last(result-str, next.character);
	    make(<parsed-string>, string: as(<string>, result-str));
	  elseif (result-str.size = 1)
	    regexp.right := optimize(regexp.right);
	    regexp;
	  else
	    make(<alternative>,
		 left: make(<parsed-string>, string: as(<string>, result-str)),
		 right: optimize(next));
	  end if;
	end for;
      else
	regexp.left := optimize(regexp.left);
	regexp.right := optimize(regexp.right);
	regexp;
      end if;
    <union> =>
      regexp.left := optimize(regexp.left);
      regexp.right := optimize(regexp.right);
      regexp;
    <quantified-atom> =>
      regexp.atom := optimize(regexp.atom);
      regexp;
    otherwise =>
      regexp;
  end select;
end method optimize;

// We have to somehow deal with pathological regular expressions like
// ".**".  Perl simply signals an error in this case.  We *could* in
// fact match these pathological regexps using the formulation below,
// but it doesn't seem worth the trouble.  Frankly, I doubt anyone has
// ever tried to use such a pathological regexp and *not* have done it
// by mistake.  But in case I'm wrong, here's how to fix a
// pathological regexp:
//
// First, realize that pathological regexps stem from infinitely
// quantifying subregexps that could match the empty string.  So what
// we do is find this subregexps, and perform the following
// transformation:
//
//  case (type of regexp)
//    r1r2 => r1'r2|r2'
//    r1|r2 => r1'|r2'
//    r1{0,n} => r1'{1,n}
//    r1{0,} => r1'{1,}
//    atom => atom
//    assertion => can't be done
//
// This transformation turns a might-match-emptystring regexp into a
// regexp that matches the same set of strings minus the empty string.
// If this transformation can't be done, remember that "$*" is
// equivalent to "always true and consumes no input".


define generic matches-empty-string? (regexp :: <parsed-regexp>)
 => answer :: <boolean>;

define method matches-empty-string? (regexp :: <parsed-atom>)
 => answer :: <boolean>;
  #f;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <parsed-assertion>)
 => answer :: <boolean>;
  #t;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <mark>)
 => answer :: <boolean>;
  regexp.child.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <union>)
 => answer :: <boolean>;
  regexp.left.matches-empty-string? | regexp.right.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <alternative>)
 => answer :: <boolean>;
  regexp.left.matches-empty-string? & regexp.right.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <quantified-atom>)
 => answer :: <boolean>;
   regexp.min-matches == 0 | regexp.atom.matches-empty-string?;
end method matches-empty-string?;


define generic pathological? (regexp :: <parsed-regexp>)
 => answer :: <boolean>;

define method pathological? (regexp :: <parsed-atom>)
 => answer :: <boolean>;
  #f;
end method pathological?;

define method pathological? (regexp :: <parsed-assertion>)
 => answer :: <boolean>;
  #f;
end method pathological?;

define method pathological? (regexp :: <mark>)
 => answer :: <boolean>;
  regexp.child.pathological?;
end method pathological?;

define method pathological? (regexp :: <union>)
 => answer :: <boolean>;
  regexp.left.pathological? | regexp.right.pathological?;
end method pathological?;

define method pathological? (regexp :: <alternative>)
 => answer :: <boolean>;
  regexp.left.pathological? | regexp.right.pathological?;
end method pathological?;

define method pathological? (regexp :: <quantified-atom>)
 => answer :: <boolean>;
  regexp.max-matches == #f & regexp.atom.matches-empty-string?;
end method pathological?;

// Seals for file parse.dylan

// <mark> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<mark>));
// <union> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<union>));
// <alternative> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<alternative>));
// <parsed-assertion> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<parsed-assertion>));
// <quantified-atom> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<quantified-atom>));
// <parsed-character> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-character>));
// <parsed-string> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-string>));
// <parsed-set> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-set>));
// <parsed-backreference> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-backreference>));
// <parse-info> -- subclass of <object>
define sealed domain make(singleton(<parse-info>));
define sealed domain initialize(<parse-info>);
