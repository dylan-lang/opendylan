module: locator-internals
author: Tim McNerney
revised: 28-Dec-95 mf
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method wild-locator? 
    (x :: <object>) => (wild? :: <boolean>)
  #f
end method;

define method wild-locator? 
    (x == #"wild") => (wild? :: <boolean>)
  #t
end method;

define method wild-locator?
    (x == #"wild-inferiors") => (wild? :: <boolean>)
  #t
end method;

/*
define method wild-locator? 
    (x == #f) => (wild? :: <boolean>)
  #f
end method;

define method wild-locator? 
    (x == #"not-applicable") => (wild? :: <boolean>)
  #t
end method;

define method wild-locator? 
    (x :: <string>) => (wild? :: <boolean>)
  #f   // for now, until we implement a real matcher
end method;
*/

// Generic matching

define method match 
    (pattern :: <object>, candidate :: <object>) 
 => (result :: <object>)
  if (pattern = candidate)
    candidate
  else
    #"fail"
  end if
end method;

define method match 
    (pattern == #"wild", candidate :: <object>) 
 => (result :: <object>)
  candidate
end method;

// Generic instantiation

define method instantiate
    (pattern :: <object>, match :: <object>) => (object)
  pattern
end method;

define method instantiate 
    (pattern == #"wild", match :: <object>) => (object)
  match
end method;

//////////////////////

// Was commented out using two sets of balanced comment delimiters
// This doesn't seem to work.

//define generic match (candidate :: <locator>, pattern :: <locator>)
//                     => result :: false-or(<locator>);
//  // a.k.a. subset?(candidate, pattern)
//  // returns a locator which represents an environment with the wildcards
//  // bound to what they match (handwave, handwave)
//  // else => #f

//define class <locator-map> (<object>)
//  slot from :: <locator>;
//  slot to :: <locator>;
//end class;

