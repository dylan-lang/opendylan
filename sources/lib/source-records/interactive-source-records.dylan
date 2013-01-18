Module: source-records-implementation
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <interactive-source-record> (<source-record>)
  slot interactive-source-record-id = #f,
    init-keyword: id:;
  constant slot source-record-project :: <object>,
    required-init-keyword: project:;
  constant slot source-record-module-name :: <symbol>,
    required-init-keyword: module:;
  constant slot source-record-language :: <symbol> = #"infix-dylan",
    init-keyword: language:;
  constant slot source-record-contents :: <byte-vector>,
    required-init-keyword: source:;
end class;

define method source-record-start-line
    (record :: <interactive-source-record>) => (line :: <integer>)
  0
end method;

define method source-record-location
    (record :: <interactive-source-record>, #key check-if-exists?)
 => (location :: singleton(#f))
  #f
end method;

/*
define method id-as-source-record
    (class == <interactive-source-record>, project, location, id)
 => (sr :: <interactive-source-record>)
  make(class, project: project, module: #"dummy", source: "dummy", id: id)
end method;
*/

define method call-with-source-record-input-stream
    (fn :: <function>, sr :: <interactive-source-record>, #key)
 => (#rest fn-values)
  let stream = make(<sequence-stream>,
                    contents: sr.source-record-contents);
  fn(stream)
end method;


define method source-record-name
    (sr :: <interactive-source-record>) => (name :: false-or(<string>))
  #f
end method;

