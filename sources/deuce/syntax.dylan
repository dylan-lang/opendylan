Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Syntax tables

define protocol <<syntax-table>> ()
  getter character-syntax
    (character :: <character>, syntax-table :: <syntax-table>)
 => (syntax :: <integer>);
  setter character-syntax-setter
    (syntax :: <integer>, character :: <character>, syntax-table :: <syntax-table>)
 => (syntax :: <integer>);
end protocol <<syntax-table>>;

// The largest character code in straight ASCII (ISO Latin-1)
define constant $largest-byte-character-code :: <integer> = 255;

// A syntax table maps from a character code to a lexical syntax code
define sealed class <syntax-table> (<object>)
  // A vector that's just big enough to hold the byte characters  
  sealed slot %syntax-table :: <simple-object-vector>
    = make(<simple-object-vector>,
	   size: $largest-byte-character-code + 1, fill: -1);
end class <syntax-table>;

define sealed domain make (singleton(<syntax-table>));
define sealed domain initialize (<syntax-table>);


define sealed method copy-syntax-table
    (syntax-table :: <syntax-table>)
 => (new-syntax-table :: <syntax-table>)
  copy-syntax-table-into!(syntax-table, make(<syntax-table>))
end method copy-syntax-table;

define sealed method copy-syntax-table-into!
    (syntax-table :: <syntax-table>, into :: <syntax-table>)
 => (into :: <syntax-table>)
  replace-subsequence!(into.%syntax-table, syntax-table.%syntax-table);
  into
end method copy-syntax-table-into!;


define sealed method character-syntax
    (character :: <byte-character>, syntax-table :: <syntax-table>)
 => (syntax :: <integer>)
  let code = as(<integer>, character);
  if (code < 0 | code > $largest-byte-character-code)
    -1
  else
    syntax-table.%syntax-table[code]
  end
end method character-syntax;

define sealed method character-syntax-setter
    (syntax :: <integer>, character :: <byte-character>, syntax-table :: <syntax-table>)
 => (syntax :: <integer>)
  let code = as(<integer>, character);
  when (code >= 0 & code <= $largest-byte-character-code)
    syntax-table.%syntax-table[code] := syntax
  end;
  syntax
end method character-syntax-setter;


// Word syntax constants
define constant $word-alphabetic = 0;	// the char is alphanumeric
define constant $word-delimiter  = 1;	// the char is a delimiter

// Word syntax constants
define constant $atom-alphabetic = 0;	// the char is alphanumeric
define constant $atom-delimiter  = 1;	// the char is a delimiter

assert($word-alphabetic == $atom-alphabetic & $word-delimiter == $atom-delimiter,
       "Atom syntax constants inconsistent");

// List syntax constants
define constant $list-alphabetic   = 0;
define constant $list-delimiter    = 1;
define constant $list-escape       = 2;	// "quotes" the next character
define constant $list-single-quote = 3;	// a single quote, might or might not act like a double quote
define constant $list-double-quote = 4;	// a double quote, starts a grouping terminated by another double quote
define constant $list-open         = 5;	// an open parenthesis or bracket
define constant $list-close        = 6;	// an close parenthesis or bracket

assert($word-alphabetic == $list-alphabetic & $word-delimiter == $list-delimiter,
       "List syntax constants inconsistent");


/// Default syntax tables

define constant $default-word-syntax :: <syntax-table> = make(<syntax-table>);
define constant $default-atom-syntax :: <syntax-table> = make(<syntax-table>);
define constant $default-list-syntax :: <syntax-table> = make(<syntax-table>);

define function initialize-syntax-tables () => ()
  // Ordinary word syntax table
  // Most things are delimiters, except '0' to '9', 'A' to 'Z', and 'a' to 'z'
  let table = $default-word-syntax.%syntax-table;
  fill!(table, $word-delimiter,  start:  32, end: 128);
  fill!(table, $word-alphabetic, start:  48, end:  58);
  fill!(table, $word-alphabetic, start:  65, end:  91);
  fill!(table, $word-alphabetic, start:  97, end: 123);
  fill!(table, $word-alphabetic, start: 128, end: 256);
  table[as(<integer>, '\t')] := $word-delimiter;
  table[as(<integer>, '\n')] := $word-delimiter;
  table[as(<integer>, '\r')] := $word-delimiter;
  table[as(<integer>, '\f')] := $word-delimiter;
  // Atom word syntax table, prejudiced towards Dylan
  copy-syntax-table-into!($default-word-syntax, $default-atom-syntax);
  let table = $default-atom-syntax.%syntax-table;
  table[as(<integer>, '!')] := $atom-alphabetic;
  table[as(<integer>, '"')] := $atom-alphabetic;
  table[as(<integer>, '#')] := $atom-alphabetic;
  table[as(<integer>, '$')] := $atom-alphabetic;
  table[as(<integer>, '%')] := $atom-alphabetic;
  table[as(<integer>, '&')] := $atom-alphabetic;
  table[as(<integer>, '*')] := $atom-alphabetic;
  table[as(<integer>, '+')] := $atom-alphabetic;
  table[as(<integer>, '-')] := $atom-alphabetic;
  table[as(<integer>, '/')] := $atom-alphabetic;
  table[as(<integer>, ':')] := $atom-alphabetic;
  table[as(<integer>, '<')] := $atom-alphabetic;
  table[as(<integer>, '=')] := $atom-alphabetic;
  table[as(<integer>, '>')] := $atom-alphabetic;
  table[as(<integer>, '?')] := $atom-alphabetic;
  table[as(<integer>, '^')] := $atom-alphabetic;
  table[as(<integer>, '_')] := $atom-alphabetic;
  table[as(<integer>, '|')] := $atom-alphabetic;
  table[as(<integer>, '~')] := $atom-alphabetic;
  // List syntax table, prejudiced towards Dylan
  copy-syntax-table-into!($default-atom-syntax, $default-list-syntax);
  let table = $default-list-syntax.%syntax-table;
  table[as(<integer>, '"')]  := $list-double-quote;
  table[as(<integer>, '\'')] := $list-double-quote;	// in Dylan, this acts like a double quote
  table[as(<integer>, '\\')] := $list-escape;
  table[as(<integer>, '#')]  := $list-single-quote;
  table[as(<integer>, '(')]  := $list-open;
  table[as(<integer>, ')')]  := $list-close;
  table[as(<integer>, '[')]  := $list-open;
  table[as(<integer>, ']')]  := $list-close;
  table[as(<integer>, '{')]  := $list-open;
  table[as(<integer>, '}')]  := $list-close;
  #f
end function initialize-syntax-tables;

initialize-syntax-tables();
