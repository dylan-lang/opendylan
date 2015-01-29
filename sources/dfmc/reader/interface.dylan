Module:    dfmc-reader
Synopsis:  The primary interfaces for driving the parser, one for reading
           source records a top level constituent fragment at a time, and
           one for re-parsing used by the macro system.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Read a single top level constituent.

define compiler-open generic read-top-level-fragment
    (record :: <compilation-record>, lexer :: false-or(<lexer>))
 => (fragment, lexer);

define method read-top-level-fragment
    (record :: <compilation-record>, lexer :: false-or(<lexer>))
 => (fragment, lexer)
  with-classification-cache
    let lexer
      = lexer | make(<lexer>,
                     source: record,
                     start-posn: 0,
                     start-line: 1,
                     line-start: 0);
    local method lex ()
      let fragment = get-token(lexer);
      values(fragment-kind(fragment), fragment, fragment)
    end method;
    dynamic-bind (*fragment-context* = compilation-record-module(record))
      block ()
        let fragment
          = run-parser(#f, dylan-parser, lex,
                       // make(<parser-lexer>, function: lex),
                       on-error: parser-error-handler);
        values(fragment, lexer);
        // This only parses, discarding most forms:
        /*
        for (f = run-parser(#f, dylan-parser, lex, on-error: parser-error-handler)
             then run-parser(#f, dylan-parser, lex, on-error: parser-error-handler),
             until: (~f
                       | instance?(f, <macro-body-definition-fragment>)
                       | instance?(f, <function-call-fragment>)))
        finally
          values(f, lexer);
        end;
        */
      exception (ex :: <reader-error>)
        signal(ex);
        skip-to-next-top-level-form(lexer);
        read-top-level-fragment(record, lexer);
      end;
    end dynamic-bind;
  end with-classification-cache;
end method read-top-level-fragment;

define function source-lines-read (lexer :: <lexer>) => (lines :: <integer>)
  (lexer.line | -1) + 1
end function;

// Re-read using a given lexer function.

define function re-read-fragments
    (lexer :: <function>, #key on-error = parser-error-handler) => (form)
  local method inner-lexer ()
    let frag = lexer();
    values(fragment-kind(frag), frag, frag);
  end method;
  run-parser(#f, dylan-parser, inner-lexer, on-error: on-error);
end function;

define open abstract serious-program-warning <reader-error>
end serious-program-warning;

define serious-program-warning <invalid-token> (<reader-error>)
  slot condition-token-string,
    required-init-keyword: token-string:;
  format-string
    "Invalid token beginning %= encountered.";
  format-arguments token-string;
end serious-program-warning;

define function invalid-token (lexer-location)
  let string = extract-string(lexer-location);
  let location = lexer-location-source-location(lexer-location);
  note(<invalid-token>,
       source-location: location,
       token-string: string);
end function;

define serious-program-warning <integer-too-large> (<invalid-token>)
  format-string
    "The integer %s is too large for any available integer representation.";
  format-arguments token-string;
end serious-program-warning;

define serious-program-warning <character-code-too-large> (<invalid-token>)
  format-string
    "Hex escape code in %s is too large to be represented as "
    "a byte character.";
  format-arguments token-string;
end serious-program-warning;

define serious-program-warning <ratios-not-supported> (<invalid-token>)
  format-string
    "The ratio %s cannot be read because no ratio representation is "
    "provided.";
  format-arguments token-string;
end serious-program-warning;

define serious-program-warning <invalid-end-of-input> (<reader-error>)
  format-string
    "Unexpected end of input encountered while reading form.";
end serious-program-warning;

define function invalid-end-of-input (lexer-location)
  let location = lexer-location-source-location(lexer-location);
  note(<invalid-end-of-input>,
       source-location: location);
end function;

define function lexer-location-source-location (lexer-location)
  record-position-as-location
    (source-location-record(lexer-location),
     source-location-source-position(lexer-location));
end function;

define serious-program-warning <parser-error> (<reader-error>)
  slot condition-token-string,
    required-init-keyword: token-string:;
  format-string "Unexpected token %=.";
  format-arguments token-string;
end serious-program-warning;

define method parser-error-handler (token-type, fragment, history)
  let location = fragment-source-location(fragment);
  let string
    = if (location)
        extract-token-text(location)
      else
        ""
      end;
  note(<parser-error>,
       source-location: location,
       token-string: string);
end method;

define method parser-error-handler
    (token-type, fragment :: <eof-marker>, history)
  let position = position-between(last(history), fragment);
  let location
    = record-position-as-location(fragment-record(fragment), position);
  note(<invalid-end-of-input>,
       source-location: location);
end method;

// The superclass of parse errors spotted "manually" during later
// procedural processing.

define open abstract serious-program-warning <manual-parser-error>
    (<reader-error>)
end serious-program-warning;
