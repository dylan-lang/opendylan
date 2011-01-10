Module: source-records-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <line-source-location> (<source-location>)
  constant slot source-location-source-record,
    required-init-keyword: source-record:;
  constant slot source-location-start-offset :: <source-offset>,
    required-init-keyword: start-offset:;
end class;

define class <line-source-offset> (<big-source-offset>)
  constant slot source-offset-line :: <integer>,
    required-init-keyword: line:;
end class;

define method source-location-end-offset 
    (loc :: <line-source-location>) => (offset :: <line-source-offset>)
  source-location-start-offset(loc)
end method;

define method source-offset-column 
    (offset :: <line-source-offset>) => (pos :: <integer>)
  0
end method;

define method make-line-location 
    (sr, line) => (loc :: <line-source-location>)
  let offset = make(<line-source-offset>, line: line);
  make(<line-source-location>,
       source-record: sr,
       start-offset:  offset)
end method;
