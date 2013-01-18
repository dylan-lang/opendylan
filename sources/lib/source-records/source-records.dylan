Module: source-records-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <source-record> (<object>)
end class;

define constant <source-record-sequence>
  = limited(<sequence>, of: <source-record>);

define open abstract class <source-record-error> (<simple-condition>, <error>) end;

// Source record id's.  The compiler requires the source record id be
// coercible to a string with no embedded newlines.
define open generic source-record-as-id (sr :: <source-record>, location)
 => id;

// Create a new source record with the specified id.
define open generic id-as-source-record (class :: subclass(<source-record>),
                                         project :: <object>,
                                         location,
                                         id)
 => sr :: <source-record>;

// A hack to avoid more pervasive source changes, but allow some calls to bypass
// the timestamp check
define thread variable *check-source-record-date?* = #t;

define open generic call-with-source-record-input-stream
    (fn :: <function>, sr :: <source-record>,
     #key, #all-keys)
 => (#rest fn-values);

define open generic source-record-project (sr :: <source-record>)
 => project :: <object>;

define open generic source-record-module-name (sr :: <source-record>)
 => module-name :: <symbol>;

define open generic source-record-language (sr :: <source-record>)
 => module-name :: <symbol>;

define open generic source-record-contents (sr :: <source-record>)
 => contents :: <sequence>;

// Optional compiler callback.  This can return #f and the compiler
// will cope, but it's nicer if there is a meaningful name.  If a name
// is returned, it must be unique among all the source records in a project
// (it's used to name output files).
define open generic source-record-name (sr :: <source-record>)
 => name :: false-or(<string>);

define method source-record-name (sr :: <source-record>)
    => (name :: singleton(#f));
  #f
end method;

define open generic source-record-relative-name (sr :: <source-record>, location)
 => (name :: false-or(<string>));

define method source-record-relative-name (sr :: <source-record>, location)
 => (name :: singleton(#f))
  #f
end method;

// Compiler callbacks for condition printing.  Default method prints using
// source record name
define open generic print-source-line-location
    (sr :: <source-record>, line, stream) => ();
define open generic source-line-location
    (sr :: <source-record>, line) => (name, real-line-number);

define method print-source-line-location (sr :: <source-record>, line, stream)
 => ();
  format(stream, "%s:%s", source-record-name(sr), line);
end method;

define method source-line-location (sr :: <source-record>, line)
    => (name, real-line-number);
  values(source-record-name(sr), line);
end method;

define open generic source-char-offset (sr :: <source-record>) => (offset);

define method source-char-offset (sr :: <source-record>) => (offset);
  0;
end method;

// Support for source locations stored in object files.
define open generic object-source-location-lines (sr :: <source-location>)
 => (start-line :: <integer>, end-line :: <integer>);

define method object-source-location-lines (loc :: <source-location>)
 => (start-line :: <integer>, end-line :: <integer>);
  let sr = loc.source-location-source-record;
  let offset = sr.source-record-start-line;
  let start-line = loc.source-location-start-offset.source-offset-line;
  let end-line = loc.source-location-end-offset.source-offset-line;
  values(offset + start-line, offset + end-line)
end;

// this error can be detected when we have a source record already
// (but it was removed from storage)
// or when we are trying to create the record for the first time
define class <source-record-missing> (<source-record-error>)
  constant slot source-record-missing-record,
    required-init-keyword: source-record:;
end class;

define open generic condition-source-record
    (condition :: <source-record-missing>) => (sr :: <source-record>);

define method condition-source-record (c :: <source-record-missing>)
 => sr :: <source-record>;
  any?(method (arg) instance?(arg, <source-record>) & arg end,
       condition-format-arguments(c))
end method;

define macro with-input-from-source-record
  { with-input-from-source-record (?stream:variable = ?:expression)
      ?:body
    end }
    => { call-with-source-record-input-stream
           (method (?stream) ?body end, ?expression) }
end macro;

define open generic source-record-removed?(sr :: <source-record>)
 => (yes :: <boolean>);

define open generic source-record-modified?(sr :: <source-record>)
 => (yes :: <boolean>);

// Support for condition printing.
define open generic source-record-location (sr :: <source-record>,
                                            #key check-if-exists?)
 => (location);

define open generic source-record-start-line (sr :: <source-record>)
 => (line :: <integer>);


define method source-record-start-line (sr :: <source-record>)
  => (line :: singleton(0))
  0
end;
