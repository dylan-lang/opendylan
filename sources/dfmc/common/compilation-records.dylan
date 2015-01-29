module:   dfmc-common
Synopsis: The compilation-record class and protocol.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define dood-class <compilation-record> (<dood-mapped-object>)
  // This slot contains the project library description for project
  // compilation records.
  // For interactive compilation records, this slot contains #f
  // if the compilation record is currently being compiled, or
  // it contains the interactive library description if the compilation
  // record was previously interactively compiled.
  slot compilation-record-original-library,
    required-init-keyword: library:;
  lazy slot compilation-record-module = #f;
  lazy slot compilation-record-top-level-forms :: false-or(<sequence>) = #f;
  slot compilation-record-definitions-installed? :: <boolean> = #f;
  weak slot compilation-record-model-heap = #f,
    reinit-expression: #f;
  lazy slot compilation-record-heap-referenced-objects :: false-or(<simple-object-vector>) = #f;
  slot compilation-record-needs-linking? = #t;
  slot compilation-record-sequence-number = #f;
  constant slot compilation-record-source-record,
    required-init-keyword: source-record:;
  slot compilation-record-source-line-count :: false-or(<integer>) = #f;
  // The list of those properties in library-owned-model-properties
  // associated with this record.  Only used to speed up clearing
  // the library-owned-model-properties table during retracting.
  weak slot compilation-record-model-properties = #(),
    reinit-expression: #();
  lazy slot compilation-record-dependency-table = make(<table>);
  lazy slot compilation-record-dispatch-decisions = #();
  // This table records the objects we've had to make local copies
  // of so that we don't have duplicate copies in the same compilation
  // record.
  weak slot compilation-record-inline-only-table = make(<table>),
    reinit-expression: make(<table>);
  lazy slot compilation-record-back-end-data = #f;
  slot compilation-record-approximate-model-heap-size :: <integer> = 0;
  slot compilation-record-data-size :: <integer> = 0;
  slot compilation-record-code-size :: <integer> = 0;
  // The total line count of preceding sources (in combined mode) if any,
  // for use in the back end.  Only valid during code emission.
  weak slot compilation-record-preceding-line-count :: <integer> = 0,
    reinit-expression: 0;
end;

define function clear-compilation-record-caches
    (cr :: <compilation-record>) => ()
  remove-all-keys!(compilation-record-inline-only-table(cr));
  cr.compilation-record-back-end-data := #f;
end function;

define class <interactive-compilation-record> (<compilation-record>)
  slot compilation-record-transaction-id;
end;

define method compilation-record-transaction-id (cr :: <compilation-record>)
 => transaction-id;
  #f
end method;

define inline function compilation-record-interactive?
    (cr :: <compilation-record>) => (well? :: <boolean>)
  instance?(cr, <interactive-compilation-record>)
end function;

define compiler-open generic compilation-record-library
    (cr :: <compilation-record>) => ld;

define inline function compilation-record-downloaded?
    (cr :: <compilation-record>) => (well? :: <boolean>)
  cr.compilation-record-original-library & #t
end;

define class <library-compilation-record> (<compilation-record>)
  constant slot compilation-record-name :: <byte-string>,
    required-init-keyword: name:;
end;


define constant <compilation-record-sequence>
  = limited(<sequence>, of: <compilation-record>);

define constant <compilation-record-vector>
  = limited(<vector>, of: <compilation-record>);

// Return the sequence of top-level-form objects evident in the
// source, or #f if not yet computed.

define generic compilation-record-top-level-forms
    (cr :: <compilation-record>)
 => (forms :: false-or(<sequence>));

// Return the core set of top-level-form objects after macro-expansion.

define generic compilation-record-expanded-top-level-forms
    (cr :: <compilation-record>) => (forms :: <sequence>);

// Add a derived top-level-form.

define generic add-derived-top-level-forms
    (cr :: <compilation-record>, forms :: <sequence>) => ();

define method add-derived-top-level-forms
    (cr :: <compilation-record>, forms :: <sequence>) => ()
  compilation-record-top-level-forms(cr)
    := concatenate(forms, compilation-record-top-level-forms(cr));
end method;

// Back-end protocol
define compiler-open generic retract-compilation-record-heap
    (cr :: <compilation-record>) => ();

define compiler-open generic compilation-record-name
    (cr :: <compilation-record>) => (name :: <string>);

/*
define method compilation-record-name
    (cr :: <compilation-record>) => (name :: <string>)
  source-record-name(cr.compilation-record-source-record)
    | format-to-string("SR%d", cr.compilation-record-sequence-number);
end method;
*/

define method compilation-record-name
    (cr :: <interactive-compilation-record>) => (name :: <string>)
  concatenate("ISR", integer-to-string(cr.compilation-record-sequence-number))
end method;

define open generic source-record-compilation-record
  (library, source-record, #key default) => (cr-or-default);

// Debugging
define method print-object (o :: <compilation-record>, s :: <stream>) => ()
  format(s, "{<compilation-record> %=}", compilation-record-name(o));
end method;


// Not really implemented yet.

define method immutable-model (object) => model;
  mapped-model(object)
end;

define method immutable-model (object :: <byte-string>) => model;
  // let object = mapped-model(object);
  read-only-model-properties(object);
  object
end;

define method immutable-model (object :: <model-properties>) => model;
  object
end method;
