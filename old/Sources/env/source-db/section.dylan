Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Credits: John Dunning
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <section> (<db-object>)
  slot symbol-id :: <symbol>, required-init-keyword: symbol-id:;
  slot author;
  slot comment;
  slot date;
  slot section-text :: union(singleton(#f), <byte-string>), required-init-keyword: text:;
  slot section-start :: <integer>, init-keyword: start:;
  slot section-length :: <integer>, init-keyword: length:;
  slot section-code, init-keyword: code:;
  slot referencors;		// vector of file instances
  slot predecessor :: union(<section>, singleton(#f)), 
    init-value: #f, init-keyword: predecessor:; // parent
  slot successors;		// children
end class;

define method print(s :: <section>, #key stream = #t, verbose? = #f)
  format(stream, "[section: %=][%d->%d]", s.symbol-id, s.section-start, s.section-length);
end;

define method describe(s :: <section>, #key verbose? = #f)
  if(verbose?)
    format(*standard-output*, "[section: %=][%d->%d]\n %=\n", s.symbol-id, s.section-start,
	   s.section-length, as(<string>, s.section-text));
  else
    format(*standard-output*, "[section: %=][%d->%d]\n", s.symbol-id, s.section-start,
	   s.section-length);
  end;
end;


define class <section-handle> (<db-object>)
  slot id :: <integer>, required-init-keyword: id:;
  slot section :: <section>, required-init-keyword: section:;
end;


