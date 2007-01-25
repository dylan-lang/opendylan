module:    Collection-Utilities
author:    Matthias Hölzl (tc@xantira.com)
copyright: see below
version:   0.1 10 Apr 2004
synopsis:  This Module implements some useful methods on collections.

// Copyright.
// =========

// Copyright (C) 1998-2004 Dr. Matthias Hölzl.

//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
// 
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
// 
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
// 
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".

// If you need to receive this library under another license contact
// the author (tc@xantira.com).


// SINGLETON? -- check whether ARG is a collection with a single 
// element.
//
define open generic singleton?
    (collection :: <collection>) => singleton? :: <boolean>;

define method singleton?
    (collection :: <collection>) => singleton? :: <boolean>;
  collection.size = 1;
end method singleton?;

define sealed method singleton?
    (list :: <pair>) => singleton? :: <boolean>;
  empty?(tail(list));
end method singleton?;

define sealed method singleton?
    (list :: <empty-list>) => false :: <boolean>;
  #f;
end method singleton?;

define constant $not-found = pair(#f, #f);

define method key-exists?
    (collection :: <collection>, key)
 => (key-exists? :: <boolean>, value :: <object>);
  let result = element(collection, key, default: $not-found);
  if (result)
    values(#t, result);
  else
    values(#f, result);
  end if;
end method key-exists?;
