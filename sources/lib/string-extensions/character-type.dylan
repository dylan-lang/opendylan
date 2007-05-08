module:   character-type
author:   Nick Kramer (nkramer@cs.cmu.edu)
copyright: see below

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================


// Implements character tests that are in C's ctype library, as well
// as predecessor and successor (which help preserve sanity while
// stepping through all possible characters)

// isalpha
//
define inline function alpha? (c :: <character>) => answer :: <boolean>;
  (c >= 'a' & c <= 'z')  |  (c >= 'A' & c <= 'Z');
end function alpha?;

// Another name for alpha?.  Why choose between two good names for a
// function when you can use both?
//
define inline function alphabetic? (c :: <character>) => answer :: <boolean>;
  c.alpha?
end function alphabetic?;

// isdigit
//
define inline function digit? (c :: <character>) => answer :: <boolean>;
  (c >= '0' & c <= '9');
end function digit?;

// isalnum
//
define inline function alphanumeric? (c :: <character>) => answer :: <boolean>;
  (c >= 'a' & c <= 'z')  |  (c >= 'A' & c <= 'Z')  |  (c >= '0' & c <= '9');
end function alphanumeric?;

// isspace
//
define inline function whitespace? (c :: <character>) => answer :: <boolean>;
  select (c)
    ' ', '\t', '\n', '\f', '\r' => #t;       
                        // Space, tab, newline, formfeed, carriage return
    otherwise => #f;
  end select;
end function whitespace?;

// isupper
//
// This has been moved to %Hash-Tables in order to prevent circular library
// definitions.
//
define function uppercase? (c :: <character>) => answer :: <boolean>;
  c >= 'A' & c <= 'Z';
end function uppercase?;

// islower
//
define inline function lowercase? (c :: <character>) => answer :: <boolean>;
  c >= 'a' & c <= 'z';
end function lowercase?;

// isxdigit
//
define inline function hex-digit? (c :: <character>) => answer :: <boolean>;
  (c >= '0' & c <= '9')  |  (c >= 'a' & c <= 'f')  |  (c >= 'A' & c <= 'F');
end function hex-digit?;

// isgraph -- printing character that's not space
//
define inline function graphic? (c :: <character>) => answer :: <boolean>;
  alphanumeric?(c) | punctuation?(c);
end function graphic?;

// isprint
//
define inline function printable? (c :: <character>) => answer :: <boolean>;
  graphic?(c) | whitespace?(c);
end function printable?;

// ispunct
//
define inline function punctuation? (c :: <character>) => answer :: <boolean>;
  select (c)
    ',', '.', '/', '<', '>', '?', ';', '\'', ':', '"',
    '|', '\\', '[', ']', '{', '}',
    '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
    '-', '=', '_', '+', '`', '~'
      => #t;
    otherwise => #f;
  end select;
end function punctuation?;

// iscntrl
//
define inline function control? (c :: <character>) => answer :: <boolean>;
  ~ printable?(c);
end function control?;

// byte-character? is the only function here that has no C equivalent.
//
// As soon as we accept the concept of <byte-character>, we can change
// this definition.
//
define inline function byte-character? (c :: <character>) 
 => answer :: <boolean>;
  as(<integer>, c) < 256;
end function byte-character?;

// KJP: added
// case-insensitive-equal
//
define inline function case-insensitive-equal (c1 :: <byte-character>, c2 :: <byte-character>)
 => answer :: <boolean>;
  as-lowercase(c1) == as-lowercase(c2)
end function case-insensitive-equal;


