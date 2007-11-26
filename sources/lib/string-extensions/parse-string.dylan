module:   %parse-string
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: A useful little data structure that's not useful enough to 
          export outside the library.
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

// Parse strings: A string along with some state. A parse-string
// supports two operations: lookahead and consume. lookahead(s) gets
// the next character in the parse string, while consume(s) moves the
// pointer along.
//
define class <parse-string> (<object>)
  constant slot parse-string :: <sequence>, required-init-keyword: #"string";
  slot parse-index :: <integer>, init-value: 0;
end class <parse-string>;

define method consume
    (s :: <parse-string>) => (result :: false-or(<parse-string>))
  if (s.parse-index >= size(s.parse-string))
    #f;
  else
    s.parse-index := s.parse-index + 1;
    s;
  end if;
end method consume;

define method lookahead
    (s :: <parse-string>) => (answer :: false-or(<character>))
  if (s.parse-index >= size(s.parse-string))
    #f;
  else
    s.parse-string[s.parse-index];
  end if;
end method lookahead;

define method parse-string-done?
    (s :: <parse-string>) => (answer :: <boolean>)
  s.parse-index >= s.parse-string.size;
end method parse-string-done?;


// Seals for file parse-string.dylan

// <parse-string> -- subclass of <object>
define sealed domain make(singleton(<parse-string>));
define sealed domain initialize(<parse-string>);
