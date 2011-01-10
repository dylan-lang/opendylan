Module: dylan-user
Synopsis: Library and module definitions for core source record protocol
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library source-records
  use functional-dylan;
  use collections;
  use io;
  use system;

  export source-records, source-records-implementation;
end library;

define module source-records
    create
      <source-record>,
      <source-record-error>,
      source-record-removed?,
      source-record-modified?,
      <source-record-sequence>,
      source-record-as-id,
      id-as-source-record,
      source-record-name,
      source-record-relative-name,
      print-source-line-location,
      source-record-location,
      source-record-start-line,
      source-line-location,
      source-char-offset,
      \with-input-from-source-record, call-with-source-record-input-stream,
      <source-record-missing>,
      source-record-missing-record,
      condition-source-record,
      source-record-project,
      source-record-module-name,
      source-record-contents;

  // Source location interface.

  create
    <source-location>, make-source-location,
      source-location-source-record,
      source-location-start-offset,
      source-location-end-offset,
      source-location-string,
      print-source-record-source-location,
      copy-source-location-contents,
      object-source-location-lines,
    <source-offset>, make-source-offset,
      source-offset-character-in, 
      source-offset-line,
      source-offset-column,
      <small-source-offset>,
      <big-source-offset>,
    source-location-start-character,
    source-location-start-line,
    source-location-start-column,
    source-location-end-character,
    source-location-end-line,
    source-location-end-column; 

  create
    <source-location-table>,
    source-location-hash,
    source-location-equal?;

  create
    <line-source-location>,
    <line-source-offset>,
    make-line-location;

  create
    <interactive-source-record>,
    interactive-source-record-id, interactive-source-record-id-setter;
end module;

define module source-records-implementation
  use functional-dylan;
  use threads;
  // Probably don't need all this, sort it out later
  // use collectors;
  use set;
  use byte-vector;
  use streams;
  use format;
  use print;
  use standard-io;
  use format-out;
  use operating-system;

  export *check-source-record-date?*;

  use source-records, export: all;
  //currently exported from source-records, because of hygiene glitches
  //export call-with-source-record-input-stream;

end module;
