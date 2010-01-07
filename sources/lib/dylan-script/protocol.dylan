Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open generic contents 
    (locator :: <locator>, #key) => (contents :: <object>);

define open generic contents-setter
    (contents :: <object>, locator :: <locator>, #key) 
 => (contents :: <object>);

define open generic contents-as
    (type :: <type>, locator :: <locator>, #key) => (contents :: <object>);

// Naive default method
define method contents-as 
    (type :: <type>, locator :: <locator>, #rest args, #key)
 => (contents :: <object>)
  as(type, apply(contents, locator, args))
end method;

define open generic find 
    (pattern :: <object>, container :: <object>, #key return)
 => (result);

/// Some basic string stuff

define method text-parser 
    (string :: <byte-string>) => (text :: <byte-string>)
  string
end method;

define method find
    (pattern :: <byte-string>, container :: <byte-string>, 
       #key return = boolean:)
 => (result)
  let pos = subsequence-position(container, pattern);
  if (~pos)
    #f
  else
    select (return)
      boolean: 
        => #t;
      position: 
        => pos;
      match:
        => copy-sequence(container, start: pos, end: pos + size(pattern));
      tail:
        => copy-sequence(container, start: pos);
      before:
        => copy-sequence(container, end: pos);
      after:
        => copy-sequence(container, start: pos + size(pattern));
      otherwise
        => error("Unknown return specification %= in find", return);
    end;
  end;
end method;

define method find
    (pattern :: <object>, container :: <locator>, #rest options, #key return)
 => (result)
  apply(find, pattern, locator-as-string(<byte-string>, container), options);
end method;
