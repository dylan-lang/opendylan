Module:    dfmc-reader
Synopsis:  Code in support of the lexer, mainly to do with manipulating
           and extracting information from source record contents buffers.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Lexer source locations.

// These contain a source location with line/column information for use
// in the rest of the compiler, plus a start and end character position
// used only during lexing.

define class <lexer-source-location> (<source-location>)
  slot source-location-record :: false-or(<compilation-record>) = #f;
  slot start-posn :: <integer>, 
    required-init-keyword: start-posn:;
  slot end-posn :: <integer>, 
    required-init-keyword: start-posn:;
  slot source-location-source-position = #f;
end class;

define sealed domain make (singleton(<lexer-source-location>));
define sealed domain initialize (<lexer-source-location>);

// This is not used in the reader, and hence probably not needed at all...
define method source-location-source-record 
    (location :: <lexer-source-location>) => (sr)
  let rec = source-location-record(location);
  rec & compilation-record-source-record(rec)
end method;

define method source-location-start-offset
    (location :: <lexer-source-location>) => (offset :: <integer>)
  source-position-start-offset(source-location-source-position(location))
end method;

define method source-location-end-offset
    (location :: <lexer-source-location>) => (offset :: <integer>)
  source-position-end-offset(source-location-source-position(location))
end method;

//// Extracting text from source record byte buffers.

// Extract-string is used by the lexer in token construction, and is
// performance-critical.

define inline function extract-string 
    (loc :: <lexer-source-location>, 
       #key start: the-start :: <integer> = loc.start-posn, 
            end:   the-end   :: <integer> = loc.end-posn)
 => (string :: <byte-string>)
  let bytes = the-end - the-start;
  let string :: <byte-string> 
    = make(<byte-string>, size: bytes);
  let sr :: <compilation-record> = loc.source-location-record;
  copy-bytes
    (string, 0, sr.contents, the-start, bytes);
  string
end function;

// For bootstrapping... Always #t once bootstrapped...
// TODO: ** Remove after next release **
define variable *raw-symbol-lookup?* = instance?(make-symbol, <generic-function>);

define inline function extract-symbol
    (loc :: <lexer-source-location>, 
       #key start: the-start :: <integer> = loc.start-posn, 
            end:   the-end   :: <integer> = loc.end-posn)
 => (symbol :: <symbol>)
  let data = loc.source-location-record.contents;
  if (*raw-symbol-lookup?*)
    make-symbol(data, start: the-start, end: the-end)
  else
    as(<symbol>, extract-string(loc, start: the-start, end: the-end));
  end;
end function;  

define function extract-token-text
    (loc :: <compiler-range-source-location>, 
       #key start: the-start :: <integer> 
              = loc.source-location-start-character, 
            end: the-end :: <integer>  
              = loc.source-location-end-character)
 => (string :: <byte-string>)
  let bytes = the-end - the-start;
  let string :: <byte-string> = make(<byte-string>, size: bytes);
  copy-bytes
    (string, 0, loc.source-location-record.contents, the-start, bytes);
  string
end function;

// Extract-lines is used in certain presentation/diagnostic modes for
// presenting warnings or compiled code with the corresponding source 
// annotated.

define function extract-lines 
    (loc :: <compiler-range-source-location>) 
 => (line-strings, upper-decorator, lower-decorator)
  let text = block ()
	       let cr = loc.source-location-record;
	       let ld = compilation-record-original-library(cr);
	       // Don't even try to read source in a system library
	       when (~ld | library-description-personal?(ld))
		 contents(cr)
	       end;
	     exception (<source-record-missing>)
	       #f
	     end;
  if (~text)
    values(#f, #f, #f);
  else
    let line-count
      = loc.source-location-end-offset.source-offset-line
	  - loc.source-location-start-offset.source-offset-line;
    let line-one-start-index
      = compute-line-start-character
	  (text, loc.source-location-start-offset.source-offset-line);
    collecting (line-strings)
      local method walk-lines (cursor, lines)
	if (lines <= line-count)
	  let (line-string, next-cursor) = copy-next-line(text, cursor);
	  collect-into(line-strings, line-string);
	  walk-lines(next-cursor, lines + 1);
	end;
      end;
      walk-lines(line-one-start-index, 0);
      let line-strings = collected(line-strings);
      let start-col = loc.source-location-start-offset.source-offset-column;
      let end-col = loc.source-location-end-offset.source-offset-column;
      if (line-count = 0)
	values(line-strings,
	       make-closed-upper-decorator(start-col, end-col),
	       make-closed-lower-decorator(start-col, end-col));
      else
	values(line-strings,
	       make-open-upper-decorator(start-col, size(line-strings.first)),
	       make-open-lower-decorator(end-col));
      end;
    end;
  end;
end function;

define sideways method source-offset-character-in 
    (rec :: <source-record>, offset :: <range-source-offset>) 
 => (offset :: <integer>)
  compute-line-start-character(rec.source-record-contents,
			       offset.source-offset-line)
    + offset.source-offset-column
end method;

define function compute-line-start-character 
    (string, lines :: <integer>) => (character :: <integer>)
  let line-count = 1;
  for (i from 0, char in string, until: line-count == lines)
    if (char == $newline-code)
      line-count := line-count + 1;
    end;
  finally 
    i
  end;
end function;

define function copy-next-line (string, start) => (line-string, next-start)
  let stop = size(string);
  collecting (line-string :: <string>)
    for (i from start below stop, until: string[i] == 10)
      collect-into(line-string, as(<character>, string[i]));
    finally
      values(collected(line-string), i + 1);
    end;
  end;
end function;

// These functions construct the "decorator" lines used to indicate
// the extent of a source location on a given line.

define function make-closed-upper-decorator 
    (start-col :: <integer>, end-col :: <integer>) => (decorator :: <string>)
  collecting (as <string>)
    for (i from 0 below start-col) collect(' ') end;
    for (i from start-col below end-col) collect('-') end;
  end;
end function;

define function make-closed-lower-decorator 
    (start-col :: <integer>, end-col :: <integer>) => (decorator :: <string>)
  make-closed-upper-decorator(start-col, end-col);
end function;

define function make-open-upper-decorator 
    (start-col :: <integer>, end-col :: <integer>) => (decorator :: <string>)
  make-closed-upper-decorator(start-col, end-col);
end function;

define function make-open-lower-decorator 
    (end-col :: <integer>) => (decorator :: <string>)
  make-closed-upper-decorator(0, end-col);
end function;

//// Some stubs in support of the CMU style.

// TODO: CORRECTNESS: Get these values from the back-end, since the 
// 32 bit values hardwired here aren't going to be correct for, say, 
// the Alpha.

define constant runtime-$minimum-integer = $minimum-integer;
define constant runtime-$maximum-integer = $maximum-integer;

define macro pprint-fields
  { pprint-fields (?stuff:*) } => { ?=next-method() }
end macro;

define variable *current-module* = #f;

// define constant <self-organizing-list>   = <table>;
define constant <false>                  = <boolean>;

// TODO: CORRECTNESS: This should be <abstract-integer>

define constant <extended-integer>       = <object>;

define inline function contents 
    (source :: <compilation-record>) => (bytes :: <byte-vector>)
  source-record-contents(compilation-record-source-record(source));
end function;

// eof
