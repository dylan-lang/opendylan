Module: C-lexer-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable($character-categories, $decimal-digit);

// Predicates for identifying categories of characters.  This is fairly fo
// poor and unpleasantly character set specific.  Eventually rework this
// following the same model as the escape character stuff below.

// FIX THIS SO THAT THERE ARE SPECIAL FUNCTIONS FOR THE LEXER WHICH INCLUDE
// THE END OF INPUT MARKER AND ORDINARY FUNCTIONS WHICH DON'T.  PREFACE THE
// FUNCTIONS WITH "C-" TO DISTINGUISH THEM FROM OTHER POSSIBILITIES SINCE
// THE CATEGORIES FOR C DIALECTS ARE DIFFERENT FROM OTHER LANGUAGES.
 
define constant clex-white-space? = 
  method (c :: type-union(<character>, singleton(#"eoi")))
    select(c)
      ' ', '\t', as(<character>, 11), '\r', '\f', '\n' =>  #t;
      otherwise => #f;
    end select;
  end method;

define constant clex-hex-digit? = 
  method (c :: type-union(<character>, singleton(#"eoi")))
    (c ~= #"eoi") 
      & (((c >= 'a') & (c <= 'f'))
	   | ((c >= 'A') & (c <= 'F'))
	   | ((c >= '0') & (c <= '9')))
  end method;

define constant clex-octal-digit? = 
  method (c :: type-union(<character>, singleton(#"eoi")))
    (c ~= #"eoi") 
      & ((c >= '0') & (c <= '8'))
  end method;

define constant clex-digit? = 
  method (c :: type-union(<character>, singleton(#"eoi")))
    (c ~= #"eoi") 
      & ((c >= '0') & (c <= '9'))
  end method;

define constant clex-alpha? = 
  method (c :: type-union(<character>, singleton(#"eoi")))
    (c ~= #"eoi") 
      & (((c >= 'a') & (c <= 'z'))
	   | (c = '_')
	   | ((c >= 'A') & (c <= 'Z')))
  end method;

define constant clex-alpha-not-underscore? = 
  method (c :: <character>)
    (((c >= 'a') & (c <= 'z'))
       | ((c >= 'A') & (c <= 'Z')))
  end method;

define constant clex-alphanumeric? = 
  method (c :: type-union(<character>, singleton(#"eoi")))
    (c ~= #"eoi") 
      & (((c >= 'a') & (c <= 'z'))
	   | (c = '_')
	   | ((c >= 'A') & (c <= 'Z'))
	   | ((c >= '0') & (c <= '9')))
  end method;

define constant clex-alphanumeric-not-underscore? = 
  method (c :: <character>)
      (((c >= 'a') & (c <= 'z'))
	   | ((c >= 'A') & (c <= 'Z'))
	   | ((c >= '0') & (c <= '9')))
  end method;

// Character set independent way of figuring the size for array of bit
// vectors for identifying character values and categories (well almost,
// missing vertical tab \v (ASCII 11)).  

// Convenience - make a unit string of one character -  probably should be
// elsewhere - needs to be here for now so it can be used in the constant
// initialization that follows.

define method clex-as-unit-string (character :: <character>)
  make(<string>, size: 1, fill: character)
end;

// Character set independent way of figuring the size for array of bit
// vectors for identifying character values and categories (well almost,
// missing vertical tab \v (ASCII 11)).  

// Also note the as-ing around is a work around for bug in max in the
// emulator.

define constant $largest-char =
  begin 
    let largest-char-code = 0;
    reduce(method(seed, char) max(seed, as(<integer>, char)) end method,
	   largest-char-code,
	   concatenate("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
		       "0123456789 \t\r\f\n\b!#%^&*()-_+=~[]\\|;'\"{},.<>/?_",
		       clex-as-unit-string(as(<character>, 11))))
  end;

define constant $largest-identifier-char =
  begin 
    let largest-char-code = 0;
    reduce(method(seed, char) max(seed, as(<integer>, char)) end method,
	   largest-char-code,
	   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
  end;

define constant $smallest-identifier-char =
  begin 
    let smallest-char-code = $largest-identifier-char;
    reduce(method(seed, char) min(seed, as(<integer>, char)) end method,
	   smallest-char-code,
	   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
  end;

define constant $identifier-char-range = 
  $largest-identifier-char - $smallest-identifier-char;

define constant $identifier-char-range-squared =
  $identifier-char-range * $identifier-char-range;
 
define constant $char-array-size = $largest-char + 1;    
define constant $escape-values = 
  make(<vector>, size: $char-array-size, fill: #f); 
define constant $digit-values = 
  make(<vector>, size: $char-array-size, fill: #f); 

// Use 0 as a fill because categories are bit vectors and testing them is
// by comparisons with logand.
define constant $escape-categories = 
  make(<vector>, size: $char-array-size, fill: 0); 
define constant $character-categories = 
  make(<vector>, size: $char-array-size, fill: 0); 

// categories as bit vectors for both escape and general character tables
define constant $character-escape   = #b00001;
define constant $octal-digit        = #b00010;
define constant $hex-digit          = #b00100;
define constant $decimal-digit      = #b01000;
define constant $hex-begin          = #b10000; // letter x

// Or-ed combination categories for defining escape character tables
define constant $octal-or-hex-digit = logior($octal-digit, $hex-digit);
define constant $hex-digit-or-character-escape = 
  logior($hex-digit, $character-escape);

// predicates
define macro clex-out-of-range-character?
  { clex-out-of-range-character? ( ?char:expression ) }
    => { as(<integer>, ?char) > $largest-char }
end macro;

define macro clex-escape-category
  { clex-escape-category ( ?char:expression ) }
    => { $escape-categories[ as(<integer>, ?char )] }
end macro;

define macro clex-hex-escape-category?
  { clex-hex-escape-category? ( ?category:expression ) }
    => { logand( ?category, $hex-begin ) ~= 0 }
end macro;


/* UNUSED
define macro clex-unsafe-hex-escape?
  { clex-unsafe-hex-escape? ( ?char:expression ) }
    => { clex-hex-escape-category?( clex-escape-category( ?char ) ) }
end macro;

define macro clex-hex-escape?
  { clex-hex-escape? ( ?char:expression ) }
    => { (( ~ clex-out-of-range-character?( ?char)) & clex-unsafe-hex-escape?( ?char)) }
end macro;
*/

define macro clex-character-escape-category?
  { clex-character-escape-category? ( ?category:expression ) }
    => { logand( ?category, $character-escape ) ~= 0 }
end macro;

/* UNUSED
define macro clex-unsafe-character-escape?
  { clex-unsafe-character-escape? ( ?char:expression ) }
    => { clex-character-escape-category?( clex-escape-category( ?char ) ) }
end macro;

define macro clex-character-escape?
  { clex-character-escape? ( ?char:expression ) }
    => { (( ~ clex-out-of-range-character?( ?char)) 
	    & clex-unsafe-character-escape?( ?char)) }
end macro;
*/

define macro clex-hex-escape-digit-category?
  { clex-hex-escape-digit-category? ( ?category:expression ) }
    => { logand( ?category, $hex-digit ) ~= 0 }
end macro;

define macro clex-unsafe-hex-escape-digit?
  { clex-unsafe-hex-escape-digit? ( ?char:expression ) }
    => { clex-hex-escape-digit-category?( clex-escape-category( ?char ) ) }
end macro;

define macro clex-hex-escape-digit?
  { clex-hex-escape-digit? ( ?char:expression ) }
    => { (( ~ clex-out-of-range-character?( ?char)) 
	    & clex-unsafe-hex-escape-digit?( ?char)) }
end macro;

define macro clex-octal-escape-digit-category?
  { clex-octal-escape-digit-category? ( ?category:expression ) }
    => { logand( ?category, $octal-digit ) ~= 0 }
end macro;

define macro clex-unsafe-octal-escape-digit?
  { clex-unsafe-octal-escape-digit? ( ?char:expression ) }
    => { clex-octal-escape-digit-category?( clex-escape-category( ?char ) ) }
end macro;

define macro clex-octal-escape-digit?
  { clex-octal-escape-digit? ( ?char:expression ) }
    => { (( ~ clex-out-of-range-character?( ?char)) 
	    & clex-unsafe-octal-escape-digit?( ?char)) }
end macro;

define macro character-escape-value
  { character-escape-value ( ?char:expression ) }
    => { $escape-values[as(<integer>, ?char)] }
end macro;

define macro escape-values-definer
  { define escape-values ?maps:* end }
    => { ?maps }
maps:
  { }
    => { }
    { ?character:expression => ?value:expression; ... }
    => { $escape-values[as(<integer>, ?character)] := ?value; ...}
end macro;

define escape-values
  'a'  => '\a'; // bell
  'b'  => '\b'; // bs
  'f'  => '\f'; // form feed
  'n'  => '\n';
  'r'  => '\r';
  't'  => '\t';
  'v'  => as(<character>, 11); // vt -- hack
  '\'' => '\'';
  '"'  => '"'; 
  '?'  => '?';
  '\\' => '\\';
end escape-values;

define macro clex-digit-to-integer
  { clex-digit-to-integer ( ?char:expression ) }
    => { $digit-values [ as( <integer>, ?char )] }
end macro;

define macro clex-digit-to-machine-word
  { clex-digit-to-machine-word ( ?char:expression ) }
    => { as(<machine-word>, clex-digit-to-integer( ?char )) }
end macro;

define macro digit-values-definer
  { define digit-values ?maps:* end }
    => { ?maps }
maps:
  { }
    => { }
    { ?character:expression => ?value:expression; ... }
    => { $digit-values[as(<integer>, ?character)] := ?value; ... }
end macro;

define digit-values
  '0'  =>  0;
  '1'  =>  1;
  '2'  =>  2;
  '3'  =>  3;
  '4'  =>  4;
  '5'  =>  5;
  '6'  =>  6;
  '7'  =>  7;
  '8'  =>  8;
  '9'  =>  9;
  'A'  => 10;
  'a'  => 10;
  'B'  => 11;
  'b'  => 11;
  'C'  => 12;
  'c'  => 12;
  'D'  => 13;
  'd'  => 13;
  'E'  => 14;
  'e'  => 14;
  'F'  => 15;
  'f'  => 15;
end digit-values;     

define constant $digit-to-character-vector = make(<vector>, size: 16);

define macro clex-digit-to-character
  { clex-digit-to-character ( ?digit:expression ) }
    => { $digit-to-character-vector [ ?digit ] }
end macro;

/* UNUSED
define macro clex-machine-word-digit-to-character
  { clex-machine-word-digit-to-character ( ?digit:expression ) }
    => { clex-digit-to-character( as(<integer>, ?digit) )}
end macro;
*/

define macro digit-characters-definer
  { define digit-characters ?maps:* end }
    => { ?maps }
maps:
  { }
    => { }
  { ?digit:expression => ?character:expression; ... }
    => { $digit-to-character-vector[ ?digit ] := ?character; ... }
end macro;

define digit-characters
  0   => '0';
  1   => '1';
  2   => '2';
  3   => '3';
  4   => '4';
  5   => '5';
  6   => '6';
  7   => '7';
  8   => '8';
  9   => '9';
  10  => 'A';
  11  => 'B';
  12  => 'C';
  13  => 'D';
  14  => 'E';
  15  => 'F';
end digit-characters;     

define method clex-integer-to-string 
    (the-integer :: <integer>,
     #key base :: limited(<integer>, min: 2, max: 16) = 10)
 => (the-string :: <string>);
  let the-list = make(<t-list>);
  let was-negative? :: <boolean> =
    if (negative?(the-integer))
      the-integer := negative(the-integer);
      #t
    end if;
  while (the-integer > base)
    let (the-new-integer, the-digit) = floor/(the-integer, base); 
    push(the-list, clex-digit-to-character(the-digit));
    the-integer := the-new-integer;
  end while;
  push(the-list, clex-digit-to-character(the-integer));
  if (was-negative?) push(the-list, '-'); end if;
  as(<string>, the-list)
end method;

define macro escape-categories-definer
  { define escape-categories ?maps:* end }
    => { ?maps }
maps:
  { }
    => { }
    { ?character:expression => ?category:expression; ... }
    => { $escape-categories[as(<integer>, ?character)] := ?category; ... }
end macro;

define escape-categories
  'n'  => $character-escape;
  'r'  => $character-escape;
  't'  => $character-escape;
  'v'  => $character-escape; // vt
  '\'' => $character-escape;
  '"'  => $character-escape; 
  '?'  => $character-escape;
  '\\' => $character-escape;
  '0'  => $octal-or-hex-digit;
  '1'  => $octal-or-hex-digit;
  '2'  => $octal-or-hex-digit;
  '3'  => $octal-or-hex-digit;
  '4'  => $octal-or-hex-digit;
  '5'  => $octal-or-hex-digit;
  '6'  => $octal-or-hex-digit;
  '7'  => $octal-or-hex-digit;
  '8'  => $hex-digit;
  '9'  => $hex-digit;
  'A'  => $hex-digit;
  'a'  => $hex-digit-or-character-escape; // bell
  'B'  => $hex-digit;
  'b'  => $hex-digit-or-character-escape; // bs
  'C'  => $hex-digit;
  'c'  => $hex-digit;
  'D'  => $hex-digit;
  'd'  => $hex-digit;
  'E'  => $hex-digit;
  'e'  => $hex-digit;
  'F'  => $hex-digit;
  'f'  => $hex-digit-or-character-escape; // form feed
  'x'  => $hex-begin;
end escape-categories;     
