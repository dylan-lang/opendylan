module:     dylan-user
author:     Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Contains the library and module definitions for the String
            Extensions library.
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


define library string-extensions
  use functional-dylan;
  // use collection-extensions;
  // use table-extensions;
  export
    string-conversions, character-type, string-hacking, substring-search, 
    // These last two are only to be used by the Regular-expressions library
    %parse-string, %do-replacement;  
end library string-extensions;


// Except for the names, these are identical to the functions found in
// the C library ctype.
//
define module character-type
  use functional-dylan;
  // use extensions;
  // use %Hash-Tables, export: {uppercase?};
  export
    case-insensitive-equal, // KJP: added
    alpha?, alphabetic?, digit?, alphanumeric?, whitespace?, uppercase?,
    lowercase?, hex-digit?, graphic?, printable?,
    punctuation?, control?, byte-character?;
end module character-type;

// For internal use and for use by the regexp library
//
define module %parse-string
  use functional-dylan;
  // use extensions;
  export <parse-string>, parse-string, parse-index, consume, lookahead, parse-string-done?;
end module %parse-string;


// Contains various useful string and character functions
//
define module string-hacking
  use functional-dylan;
  // use extensions;
  use character-type;
  use %parse-string;
  // Re-export case-insensitive-equal from table-extensions
  /* KJP: removed
  use table-extensions,
    import: {case-insensitive-equal},
    export: {case-insensitive-equal};
  */
  export
    predecessor, successor, add-last,
    <character-set>, <case-sensitive-character-set>, 
    <case-insensitive-character-set>, 
    <byte-character-table>;
end module string-hacking;


// Contains various conversions that one would think would be part of
// Dylan, such as int to string and string to int.
// Also contains an "as" method for converting a character to a string.
//
define module string-conversions
  use functional-dylan,
    export: { string-to-integer, integer-to-string }; // KJP: added
  /*
  use extensions, 
    import: {<general-integer>, <extended-integer>, $maximum-integer,
	       $minimum-integer};
  */
  use string-hacking;
  use character-type;
  export
    // string-to-integer, integer-to-string, // KJP: removed
    digit-to-integer, integer-to-digit;
end module string-conversions;


// For internal use and for use by the regexp library
//
define module %do-replacement
  use functional-dylan;
  // use extensions;
  use character-type;
  use string-conversions;
  export do-replacement;
end module %do-replacement;


// Robert's Boyer-Moore implementation
//
define module substring-search
  use functional-dylan;
  use character-type; // KJP: added
  // use extensions;
  /* KJP: removed
  use subseq;
  */
  use string-hacking;
  use %do-replacement;
  export
    substring-position, make-substring-positioner, 
    substring-replace, make-substring-replacer;
end module substring-search;
