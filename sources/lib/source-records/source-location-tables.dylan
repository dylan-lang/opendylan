Module: source-records-implementation
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Protocol

define open class <source-location-table> (<table>)
end class;

define open generic source-location-hash (loc :: <source-location>, state)
 => (id :: <integer>, state);

define open generic source-location-equal? (loc1 :: <source-location>, loc2 :: <source-location>)
 => (equal? :: <boolean>);

define method table-protocol (table :: <source-location-table>)
 => (test :: <function>, hash :: <function>)
  values(source-location-equal?, source-location-hash)
end method;

/// Implementation

define method source-location-hash (loc :: <line-source-location>, initial-state)
 => (id :: <integer>, state)
  let (sr-id, next-state) = object-hash(loc.source-location-source-record, initial-state);
  let (line-id, final-state) = object-hash(loc.source-location-start-line, next-state);
  values(merge-hash-ids(sr-id, line-id), final-state);
end method;

define method source-location-equal? (loc1 :: <line-source-location>, loc2 :: <line-source-location>)
 => (equal? :: <boolean>)
  local method equal-wrt? (f, x, y)
	  f(x) = f(y);
	end method;
  equal-wrt?(source-location-source-record, loc1, loc2)
  & equal-wrt?(source-location-start-line, loc1, loc2)
end method;

