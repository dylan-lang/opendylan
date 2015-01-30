Module:    dfmc-environment-database
Synopsis:  DFM compiler dylan object information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Symbols

define sealed method symbol-name
    (server :: <dfmc-database>, symbol :: <symbol-object>)
 => (name :: <string>)
  next-method()
end method symbol-name;


/// Strings

define sealed method string-contents
    (server :: <dfmc-database>, string :: <string-object>)
 => (contents :: <string>)
  next-method()
end method string-contents;


/// Booleans

define sealed method boolean-true?
    (server :: <dfmc-database>, boolean :: <boolean-object>)
 => (true? :: <boolean>)
  next-method()
end method boolean-true;


/// Numbers

define method number-value
    (server :: <dfmc-database>, number :: <number-object>)
 => (value :: <number>)
  next-method()
end method number-value;


/// Collections

define method collection-keys
    (server :: <dfmc-database>, collection :: <collection-object>)
 => (keys :: false-or(<sequence>))
  next-method()
end method collection-keys;

define method collection-items
    (server :: <dfmc-database>, collection :: <collection-object>)
 => (items :: false-or(<sequence>))
  next-method()
end method collection-items;

define method range-object-start
    (server :: <dfmc-database>, range :: <range-object>)
 => (start :: <number>)
  next-method()
end method range-object-start;

define method range-object-end
    (server :: <dfmc-database>, range :: <range-object>)
 => (_end :: false-or(<number>))
  next-method()
end method range-object-end;

define method range-object-by
    (server :: <dfmc-database>, range :: <range-object>)
 => (by :: <number>)
  next-method()
end method range-object-by;

define method range-object-size
    (server :: <dfmc-database>, range :: <range-object>)
 => (size :: <number>)
  next-method()
end method range-object-size;

define method pair-head
    (server :: <dfmc-database>, pair :: <pair-object>)
 => (head :: <application-object>)
  next-method()
end method pair-head;

define method pair-tail
    (server :: <dfmc-database>, pair :: <pair-object>)
 => (head :: <application-object>)
  next-method()
end method pair-tail;

