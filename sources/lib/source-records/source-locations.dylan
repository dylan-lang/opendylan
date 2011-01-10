Module: source-records-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Abstract protocol for source locations and offsets.

// Note: These may end up types rather than classes in order to pack location
// information into integers to save on allocation. That's why a dedicated
// constructor is defined.

define open abstract class <source-location> (<object>) end;

define open generic make-source-location
    (record, start-char, start-line, start-col, end-char, end-line, end-col)
      => (source-location);

define open generic source-location-source-record
    (loc :: <source-location>) => (source-record);

define open generic source-location-start-offset
    (loc :: <source-location>) => (pos :: <source-offset>);

define open generic source-location-end-offset
    (loc :: <source-location>) => (pos :: <source-offset>);

// Provide a simple text representation of a source location within the
// source record. Simple batch-mode/script-driven compilation may use this
// to indicate the location of problems to the user.
define open generic print-source-record-source-location
    (sr :: <source-record>, loc :: <source-location>, stream) => ();

//// Source offsets.

// Allow implementations to pack into integers if they wish.

define constant <small-source-offset> = <integer>;

define open abstract class <big-source-offset> (<object>) end;

define constant <source-offset> 
  = type-union(<small-source-offset>, <big-source-offset>);

define open generic make-source-offset
    (char-position, line-position, end-position)
      => (source-offset);

define open generic source-offset-character-in
    (record :: <source-record>, offset :: <source-offset>) 
 => (pos :: <integer>);

define open generic source-offset-line
    (offset :: <source-offset>) => (line :: <integer>);

define open generic source-offset-column
    (offset :: <source-offset>) => (column :: <integer>);

define method print-source-record-source-location
    (sr :: <source-record>, loc :: <source-location>, stream) => ();
  let start-offset = source-location-start-offset(loc);
  let start-line = source-offset-line(start-offset);
  print-source-line-location(sr, start-line, stream);
end method;

//// Convenience functions.

define function source-location-start-character 
   (loc :: <source-location>) => (character :: <integer>)
  source-offset-character-in
    (loc.source-location-source-record,
     loc.source-location-start-offset)
end function;

define function source-location-start-line 
   (loc :: <source-location>) => (line :: <integer>)
  loc.source-location-start-offset.source-offset-line
end function;

define function source-location-start-column 
   (loc :: <source-location>) => (col :: <integer>)
  loc.source-location-start-offset.source-offset-column
end function;

define function source-location-end-character 
   (loc :: <source-location>) => (character :: <integer>)
  source-offset-character-in
    (loc.source-location-source-record,
     loc.source-location-end-offset)
end function;

define function source-location-end-line 
   (loc :: <source-location>) => (line :: <integer>)
  loc.source-location-end-offset.source-offset-line
end function;

define function source-location-end-column 
   (loc :: <source-location>) => (col :: <integer>)
  loc.source-location-end-offset.source-offset-column
end function;

define function source-location-string
    (loc :: <source-location>) => (c :: <byte-string>)
  let record = loc.source-location-source-record;
  // TODO: this walks the file twice counting cr's.  Should have a single
  // function that returns both start and end.
  let start-pos = loc.source-location-start-character;
  let end-pos = loc.source-location-end-character;
  let count = end-pos - start-pos;
  let string = make(<byte-string>, size: count);
  copy-bytes(string, 0, source-record-contents(record), start-pos, count);
  string
end;


define function copy-source-location-contents
    (loc :: <source-location>, #key check-date? = #f) => (c :: <sequence>)
  let record = loc.source-location-source-record;
  dynamic-bind(*check-source-record-date?* = check-date?)
    // TODO: this walks the file twice counting cr's.  Should have a single
    // function that returns both start and end.
    let start-offset = loc.source-location-start-character;
    let end-offset = loc.source-location-end-character;
    // TODO: Remove this hack when the environment/DUIM handle this
    // better.
    let contents = source-record-contents(record);
    if(end-offset > size(contents))
      signal(make(<source-record-missing>,
		  source-record: record,
		  format-string: "%s has been modified.",
		  format-arguments: list(record)))
    else
      copy-sequence-removing-returns
	(source-record-contents(record), start: start-offset, end: end-offset);
    end;
   end;
end;

define constant $return-code  = as(<integer>, '\r');
define constant $newline-code = as(<integer>, '\n');

define function copy-sequence-removing-returns
    (seq :: <sequence>, 
       #key start: _start :: <integer> = 0, end: _end :: <integer> = size(seq))
 => (copy :: <sequence>)
  let original-size = _end - _start;
  let copy = make(type-for-copy(seq), size: original-size);
  let return? = #f;
  let copy-index = 0;
  for (i :: <integer> from _start below _end)
    let code = seq[i];
    if (code == $return-code)
      return? := #t;
    elseif (return?)
      if (code == $newline-code)
        copy-index := copy-index - 1;
      end;
      return? := #f;
    end;
    copy[copy-index] := code;
    copy-index := copy-index + 1;
  finally
    if (copy-index == original-size)
      copy
    else
      copy-sequence(copy, end: copy-index)
    end
  end;
end function;
