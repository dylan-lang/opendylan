Module:   dfmc-reader
Synopsis: The state transition interpreter for the lexer, plus support
          routines for further parsing and token construction.
Author:   CMU, adapted by Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Derived from CMU code.
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.

define constant $max-lexer-code :: <integer> = 255;

define constant $ascii-8-bit-extensions
  = as(<string>, vector(as(<character>, 128),
                        '-',
                        as(<character>, $max-lexer-code)));

// All valid characters in source code, excludes various non-printing characters.
// (\f and \r are not required by the DRM. Nor obviously the 8-bit extensions.)
define constant $full-character-set
  = concatenate("\t\n\f\r -~", $ascii-8-bit-extensions);

define constant $octothorp-code = as(<integer>, '#');

// * #f indicates no builder, i.e., a non-accepting state.
// * A function with signature (lexer, source-location) => (fragment, newline-count)
//   makes a <fragment> for the token being accepted at this state. (The newline-count
//   return value is usually #f, indicating a token entirely on one line; no \n chars.)
// * A symbol is used for whitespace and comments, which never make it out of
//   the lexer.
define constant <fragment-builder>
  = type-union(singleton(#f), <symbol>, <function>);

// A particular state in the state machine.
define class <state> (<object>)
  // Used while building the state machine, and for debugging.
  constant slot state-name :: <symbol>,
    required-init-keyword: name:;

  constant slot state-fragment-builder :: <fragment-builder>,
    required-init-keyword: fragment-builder:;

  // Either #f or a vector of next-states indexed by character code.
  // During construction, vector elements are either state names or #f.
  // After construction, the state names are replaced by the actual
  // state objects.
  constant slot state-transitions :: false-or(<simple-object-vector>),
    required-init-keyword: transitions:;
end class <state>;

define sealed domain make (singleton(<state>));
define sealed domain initialize (<state>);

define method print-object
    (state :: <state>, stream :: <stream>) => ()
  printing-object (state, stream)
    print(state.state-name, stream);
  end;
end method;


define class <conditionally-accepting-state> (<state>)
  // Accepts a <lexer> argument and returns #t to accept, #f to not accept, and
  // #"use-previous" to use the already seen accepting state. It is an error to return
  // #"use-previous" unless you know an accepting state has already been seen by the time
  // this state is entered.
  constant slot state-action :: <function>,
    required-init-keyword: action:;
end class;

define inline method do-state-action (lexer :: <lexer>, state :: <state>)
  state.state-fragment-builder & #t
end method;

define method do-state-action (lexer :: <lexer>, state :: <conditionally-accepting-state>)
  let res = state.state-action(lexer);
  if (instance?(res, <symbol>))
    res                         // #"use-previous" or #"clear-previous"
  else
    state.state-fragment-builder & res
  end
end method;

// Make as many entries as necessary to represent the transitions from 'on' to new-state.
// 'on' can be a character, a byte-string, or #"otherwise".  If a byte-string, then it
// supports ranges: "-abc-gz" = match on dash, a, b, c through g, and z.
//
// Also check to see if this entry clashes with any earlier entries.
// If so, it means someone messed up editing the state machine.
//
define method add-transition
    (transitions :: <simple-object-vector>, on :: <character>, new-state :: <symbol>,
     #key only-if-not-set? :: <boolean>)
 => (any-set? :: <boolean>)
  let code = as(<integer>, on);
  if (transitions[code])
    if (only-if-not-set?)
      #f
    else
      error("input %= transitions to both %= and %=", on, transitions[code], new-state);
    end
  else
    transitions[code] := new-state;
    #t
  end if
end method add-transition;

define method add-transition
    (transitions :: <simple-object-vector>, on :: <byte-string>, new-state :: <symbol>,
     #key only-if-not-set? :: <boolean>)
 => (any-set? :: <boolean>)
  let any-set? = #f;
  local method add-one (char)
          let b = add-transition(transitions, char, new-state,
                                 only-if-not-set?: only-if-not-set?);
          any-set? := any-set? | b;
        end;
  let last = #f;                // Last transition actually added.
  let range = #f;               // Just saw a hyphen (but not added yet).
  for (char :: <byte-character> in on)
    if (range)
      if (last)
        for (i :: <integer> from as(<integer>, last) + 1 to as(<integer>, char))
          add-one(as(<character>, i));
        end for;
        last := #f;
      else
        add-one('-');
        add-one(char);
        last := char;
      end if;
      range := #f;
    elseif (char == '-')
      range := #t;
    else
      add-one(char);
      last := char;
    end if;
  end for;
  any-set?
end method add-transition;

define method add-transition
    (transitions :: <simple-object-vector>, on == #"otherwise", new-state :: <symbol>, #key)
 => (any-set? :: singleton(#t))
  add-transition(transitions, $full-character-set, new-state, only-if-not-set?: #t)
    | error("#\"otherwise\" => %= transition had no effect", new-state)
end method;

define function make-transition-table
    (transitions :: <sequence>) => (t :: false-or(<simple-object-vector>))
  let table = if (size(transitions) > 0)
                make(<vector>, size: $max-lexer-code + 1, fill: #f)
              end;
  let otherwise? = #f;
  for (transition in transitions,
       i from 0)
    let on = head(transition);
    if (on == #"otherwise")
      if (otherwise? | i < transitions.size - 1)
        error("state %=: there can only be one #\"otherwise\" transition "
                "and it must be last", name);
      end;
      otherwise? := #t;
    end;
    add-transition(table, on, tail(transition));
  end for;
  table
end function;

// TODO: rename to something that won't clash with local variable names, like make-state.
define method state
    (name :: <symbol>, builder :: <fragment-builder>, #rest transitions)
  let transitions = make-transition-table(transitions);
  make(<state>,
       name: name,
       fragment-builder: builder,
       transitions: transitions);
end method state;

define function conditionally-accepting-state
    (name :: <symbol>, builder :: <fragment-builder>, action :: <function>,
     #rest transitions)
  let transitions = make-transition-table(transitions);
  make(<conditionally-accepting-state>,
       name: name,
       fragment-builder: builder,
       action: action,
       transitions: transitions)
end function;

// Build a state machine and return the start state, which must be
// named #"start".
//
define method compile-state-machine
    (#rest states) => (start-state :: <state>)
  //
  // make a hash table mapping state names to states.
  //
  let state-table = make(<table>);
  for (state in states)
    let name = state.state-name;
    if (element(state-table, name, default: #f))
      error("State %= multiply defined.", name);
    else
      state-table[name] := state;
    end if;
  end for;
  //
  // Now that we have a table mapping state names to states, change the
  // entries in the transition tables to refer to the new state
  // object themselves instead of just to the new state name.
  //
  for (state in states)
    let table = state.state-transitions;
    if (table)
      for (i from 0 to $max-lexer-code)
        let new-state = table[i];
        if (new-state)
          table[i] := state-table[new-state];
        end if;
      end for;
    end if;
  end for;
  //
  // Return the start state, 'cause that is what we want
  // $initial-state to hold.
  element(state-table, #"start");
end method compile-state-machine;


// Features.

/*
define variable *features* :: <list> = #();

define method add-feature (feature :: <symbol>) => ();
  *features* := add-new!(*features*, feature);
end method add-feature;

define method remove-feature (feature :: <symbol>) => ();
  *features* := remove!(*features*, feature);
end method remove-feature;

define method feature-present? (feature :: <symbol>) => present? :: <boolean>;
  member?(feature, *features*);
end method feature-present?;
*/

define class <lexer> (<object>)
  // The compilation record we are currently tokenizing.
  // (Why is this a <compilation-record> and not a <source-record>? --cgay)
  constant slot lexer-source :: <compilation-record>,
    required-init-keyword: source:;

  // The position we are currently at in the source record.
  slot lexer-position :: <integer> = 0,
    init-keyword: position:;

  // The 1-based line number we are currently working on. (Note that column numbers you
  // may see elsewhere in the lexer are 0-based, as per tradition.)
  slot lexer-line-number :: <integer> = 1,
    init-keyword: line-number:;

  // The position at which lexer-line-number starts.  This is the position just AFTER a
  // \n character.
  slot lexer-line-start-position :: <integer> = 0,
    init-keyword: line-start-position:;

  // Tracks balanced consecutive double quotes of any length which delimit multi-quoted
  // strings. These are reset to 0 after each string is accepted.
  slot double-quote-start-count :: <integer> = 0;
  slot double-quote-end-count :: <integer> = 0;

  // Tracks nested delimited comments: /* /* ... */ */
  slot delimited-comment-start-count :: <integer> = 0;
  slot delimited-comment-end-count   :: <integer> = 0;

  // A list of tokens that have been unread.
  // slot pushed-tokens :: <list>, init-value: #();
  //
  // slot conditional-state :: false-or(<conditional-state>), init-value: #f;

  slot lexer-previous-token :: <object> = #f;

  // Tame source location object.
  constant slot tame-source-location :: <lexer-source-location>
    = make(<lexer-source-location>,
           source-record: #f,
           start-posn: 0,
           end-posn: 0);
end class <lexer>;

define sealed domain make (singleton(<lexer>));
define sealed domain initialize (<lexer>);

define method print-object
    (lexer :: <lexer>, stream :: <stream>) => ()
  printing-object (lexer, stream)
    format(stream, "source: %d bytes, pos: %d, %d:%d",
           lexer.lexer-source.contents.size,
           lexer.lexer-position,
           lexer.lexer-line-number,
           lexer.lexer-position - lexer.lexer-line-start-position);
  end;
end method;

// Used just the once.
define inline function make-lexer-source-location
    (lexer :: <lexer>, record :: <compilation-record>,
     start-char :: <integer>,
       start-line :: <integer>, start-col :: <integer>,
     end-char :: <integer>,
       end-line :: <integer>, end-col :: <integer>)
 => (loc :: <lexer-source-location>)
  let loc :: <lexer-source-location> = tame-source-location(lexer);
  loc.start-posn := start-char;
  loc.end-posn := end-char;
  loc.source-location-record := record;
  loc.source-location-source-position
    := make-range-position
         (make-source-offset(start-char, start-line, start-col),
          make-source-offset(end-char, end-line, end-col));
  loc
end function make-lexer-source-location;

define macro fragment-builder
  { fragment-builder(?:name) }
    => { method (lexer, source-location :: <lexer-source-location>)
           make(?name,
                record:
                  source-location.source-location-record,
                source-position:
                  source-location.source-location-source-position)
         end }
end macro fragment-builder;

define function get-token
    (lexer :: <lexer>) => (fragment :: <fragment>)
  // Basically, just record where we start and keep advancing the state machine until
  // there are no more possible advances.  We don't stop at the first accepting state we
  // find, because the longest token is supposed to take precedence.  We just note where
  // the last accepting state was, and when the state machine jams use that last
  // accepting state's result.
  let contents :: <byte-vector> = lexer.lexer-source.contents;
  let source-length :: <integer> = contents.size;

  let start-pos            :: <integer> = lexer.lexer-position;
  let start-line-number    :: <integer> = lexer.lexer-line-number;
  let start-line-start-pos :: <integer> = lexer.lexer-line-start-position;

  let current-line-number    :: <integer> = start-line-number;
  let current-line-start-pos :: <integer> = start-line-start-pos;

  // The hoped-for end state is that these values are not #f.
  let builder :: <fragment-builder> = #f;
  let end-pos = #f;
  let end-line-number = #f;
  let end-line-start-pos = #f;
  without-bounds-checks
    local
      method make-source-location (#key epos)
        if (epos)
          end-pos := epos;
          end-line-start-pos := current-line-start-pos;
        end;
        let start-column = start-pos - start-line-start-pos;
        let end-column = end-pos - end-line-start-pos;
        make-lexer-source-location
          (lexer, lexer.lexer-source,
           start-pos, start-line-number, start-column,
           end-pos, end-line-number | current-line-number, end-column)
      end method,
      method save-accepting-state (pos :: <integer>, bldr)
        builder := bldr;
        end-pos := pos;
        end-line-number := current-line-number;
        end-line-start-pos := current-line-start-pos;
      end method,
      method next-state (state :: <state>, pos :: <integer>)
        let table = state.state-transitions;
        if (table)
          let table :: <simple-object-vector> = table;
          let char-code :: <integer> = contents[pos];
          if (char-code == $newline-code)
            current-line-number := current-line-number + 1;
            current-line-start-pos := pos + 1;
          end;
          vector-element(table, char-code)
        end
      end method,
      method advance (state :: <state>, pos :: <integer>) => (epos :: <integer>)
        let new-state = (pos < source-length) & next-state(state, pos);
        if (new-state)
          repeat(new-state, pos + 1)
        else
          pos
        end if
      end method,
      method start-over (pos :: <integer>) => (epos :: <integer>)
        start-pos := pos;
        start-line-number := current-line-number;
        start-line-start-pos := current-line-start-pos;
        builder := #f;
        end-pos := #f;
        end-line-number := #f;
        end-line-start-pos := #f;
        repeat($initial-state, pos)
      end method,
      method repeat (state :: <state>, pos :: <integer>) => (epos :: <integer>)
        let bldr = state.state-fragment-builder;
        let acceptable = do-state-action(lexer, state);
        if (acceptable == #"clear-previous")
          builder := #f;
          end-pos := #f;
          end-line-number := current-line-number;
          end-line-start-pos := current-line-start-pos;
          advance(state, pos)
        elseif (~acceptable | ~bldr)
          advance(state, pos)
        elseif (bldr == #"whitespace")
          start-over(pos)
        elseif (bldr == #"comment")
          lexer.delimited-comment-start-count := 0;
          lexer.delimited-comment-end-count := 0;
          // TODO: Optionally attach comments to the AST. For now drop them.
          start-over(pos)
        else
          select (acceptable)
            #t =>
              save-accepting-state(pos, bldr);
              advance(state, pos);
            #"use-previous" =>
              if (builder)
                end-pos
              else
                error("the lexer expected a previous accepting state but there is none");
              end;
            #"accept" =>
              save-accepting-state(pos, bldr);
              end-pos;
            otherwise =>
              error("unexpected lexer accept action: %=", acceptable);
          end select
        end if
      end method;
    let epos = repeat($initial-state, start-pos);
    if (~builder)
      if (start-pos >= source-length)
        save-accepting-state(start-pos, fragment-builder(<eof-marker>))
      else
        invalid-token(make-source-location(epos: epos));
      end;
    end;

    // Save the current token's end position so that the next token starts here.
    lexer.lexer-position            := end-pos | start-pos + 1; // avoid infinite loops
    lexer.lexer-line-number         := end-line-number | current-line-number;
    lexer.lexer-line-start-position := end-line-start-pos | current-line-start-pos;

    // builder can be #f here if the warnings signaled above aren't handled with a
    // non-local exit.  See read-top-level-fragment for an example.
    let fragment = builder(lexer, make-source-location());
    lexer.lexer-previous-token := fragment
  end without-bounds-checks
end function get-token;

/*
define function get-token (lexer :: <lexer>) => (token :: <fragment>)
  //  block (return)
    // if (lexer.pushed-tokens ~== #())
      //
      // There are some unread tokens, so extract one of them instead of
      // consuming any more stuff from the source.
      //
    //  let result = lexer.pushed-tokens.head;
    //  lexer.pushed-tokens = lexer.pushed-tokens.tail;
    //  values(result, result.head, result.tail);
    // else
     //
      // There are no pending unread tokens, so extract the next one.
      let token = internal-get-token(lexer);
      token
    // end if;
  /*
    while (#t)
      let token = internal-get-token(lexer);
      select (token.token-kind)
        $feature-if-token =>
          let cond = parse-conditional(lexer);
          lexer.conditional-state
            := if (lexer.conditional-state.active?)
                 make(<conditional-state>, active: cond, do-else: ~cond,
                      old-state: lexer.conditional-state);
               else
                 make(<conditional-state>, active: #f, do-else: #f,
                      old-state: lexer.conditional-state);
               end if;

        $feature-elseif-token =>
          if (lexer.conditional-state == #f)
            compiler-fatal-error("#elseif with no matching #if");
          elseif (lexer.conditional-state.seen-else?)
            compiler-fatal-error("#elseif after #else in one #if");
          elseif (parse-conditional(lexer))
            lexer.conditional-state.active?
              := lexer.conditional-state.do-else?;
            lexer.conditional-state.do-else? := #f;
          else
            lexer.conditional-state.active? := #f;
          end if;

        $feature-else-token =>
          if (lexer.conditional-state == #f)
            compiler-fatal-error("#else with no matching #if");
          elseif (lexer.conditional-state.seen-else?)
            compiler-fatal-error("#else after #else in one #if");
          else
            lexer.conditional-state.seen-else? := #t;
            lexer.conditional-state.active?
              := lexer.conditional-state.do-else?;
          end if;

        $feature-endif-token =>
          if (lexer.conditional-state == #f)
            compiler-fatal-error("#endif with no matching #if");
          else
            lexer.conditional-state := lexer.conditional-state.old-state;
          end if;
        otherwise =>
          if (lexer.conditional-state.active?)
            return(token, token.source-location);
          end if;
      end select;
      return(token, token.source-location);
    end while;
  */
  // end block;
end function get-token;
*/

// unget-token -- exported.
//
// Pushes token back so that the next call to get-token will return
// it.  Used by the parser when it wants to put back its lookahead
// token.
//
/*
define method unget-token
    (lexer :: <lexer>, token :: <fragment>, srcloc :: <lexer-source-location>)
 => ();
  lexer.pushed-tokens := pair(token, pair(lexer.pushed-tokens, srcloc));
end method unget-token;
*/


// Constructors.

define method make-binary-operator
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <binary-operator-fragment>)
  make(<binary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: $binary-operator-only-token,
       name: extract-symbol(source-location),
       module: *current-module*);
end method make-binary-operator;

define method make-tilde
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <unary-operator-fragment>)
  make(<unary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       name: #"~",
       module: *current-module*);
end method make-tilde;

define method make-minus
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <unary-and-binary-operator-fragment>)
  make(<unary-and-binary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: $unary-and-binary-operator-token,
       name: #"-",
       module: *current-module*);
end method make-minus;

define method make-equal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <equal-fragment>)
  make(<equal-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position);
end method make-equal;

define method make-double-equal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <binary-operator-fragment>)
  make(<binary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: $equal-equal-token,
       name: #"==",
       module: *current-module*);
end method make-double-equal;


// Make a <quoted-name-token> for \-quoted operator.
//
define method make-quoted-name
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <variable-name-fragment>)
  make(<escaped-name-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       name: extract-symbol(source-location,
                            start: source-location.start-posn + 1),
       module: *current-module*);
end method make-quoted-name;

// Extract the name from the source location, figure out what kind of word it
// is, and make it.
//
define method make-identifier
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <variable-name-fragment>)
  let name = extract-symbol(source-location);
  let module = *current-module*;
  make(<variable-name-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: syntax-for-name(module, name),
       name: name,
       module: module);
end method make-identifier;

// We allow extended $[0-9]+ syntax for getting at history bindings.
// For now, these aren't distinguished syntactically from other names.
//
define method make-history-name
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <variable-name-fragment>)
  make-identifier(lexer, source-location);
end method make-history-name;


define method make-constrained-name
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
  let colon-posn
    = block (return)
        let contents = source-location.source-location-record.contents;
        for (posn from source-location.start-posn
               below source-location.end-posn)
          if (contents[posn] == as(<integer>, ':'))
            return(posn);
          end if;
        end for;
        error("No : in a constrained-name?");
        #f;
      end block;
  let constraint
    = extract-symbol(source-location, start: colon-posn + 1);
  let (name, explicit-name?)
    = if (colon-posn == source-location.start-posn)
        values(constraint, #f);
      else
        values(extract-symbol(source-location, end: colon-posn),
               #t);
      end;
  let prev = lexer.lexer-previous-token;
  if (~explicit-name?
        | instance?(prev, <query-fragment>)
        | instance?(prev, <query-query-fragment>)
        | instance?(prev, <escaped-substitution-fragment>))
    make(<constrained-name-fragment>,
         record: source-location.source-location-record,
         source-position: source-location.source-location-source-position,
         kind: $constrained-name-token,
         name: name,
         constraint: constraint);
  else
    make-qualified-name(lexer, source-location);
  end;
end method make-constrained-name;

define method make-qualified-name
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
  let colon1-posn
    = block (return)
        let contents = source-location.source-location-record.contents;
        for (posn from source-location.start-posn
               below source-location.end-posn)
          if (contents[posn] == as(<integer>, ':'))
            return(posn);
          end if;
        end for;
        error("No : in a constrained-name?");
        #f;
      end block;
  let colon2-posn
    = block (return)
        let contents = source-location.source-location-record.contents;
        for (posn from colon1-posn + 1
               below source-location.end-posn)
          if (contents[posn] == as(<integer>, ':'))
            return(posn);
          end if;
        finally
          posn
        end for;
      end block;
  let name
    = extract-symbol(source-location, end: colon1-posn);
  let module-name
    = extract-symbol(source-location,
                     start: colon1-posn + 1, end: colon2-posn);
  let library-name
    = colon2-posn ~= source-location.end-posn
        & extract-symbol(source-location, start: colon2-posn + 1);
  // Resolve to a normal variable name
  let module
    = resolve-qualified-variable-name-module
        (name, module-name, library-name,
           lexer-location-source-location(source-location));
  make(<variable-name-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: classify-expansion-word-in(module, name),
       name: name,
       context: module);
end method make-qualified-name;

// Return the real character that corresponds to the \-quoted
// character in a string or character literal.
//
define method escape-character
    (char :: <character>) => (escaped-char :: <character>)
  select (char)
    'a' => '\a';
    'b' => '\b';
    'e' => '\e';
    'f' => '\f';
    'n' => '\n';
    'r' => '\r';
    't' => '\t';
    '0' => '\0';
    '\\' => '\\';
    '\'' => '\'';
    '"' => '"';
  end select;
end method escape-character;

define method hex-escape-character
    (source-location :: <lexer-source-location>, start :: <integer>)
 => (char :: <character>, end-pos :: <integer>)
  let (code, epos)
    = parse-integer(source-location,
                    source-location.source-location-record.contents,
                    radix: 16, start: start, stop-at-non-digit?: #t);
  if (code > $max-lexer-code)
    note(<character-code-too-large>,
         source-location:
            record-position-as-location
              (source-location.source-location-record,
               source-location.source-location-source-position),
         token-string: extract-string(source-location));
    // If forced, continue with nul...
    values(as(<character>, 0), epos)
  else
    values(as(<character>, code), epos)
  end
end method hex-escape-character;

// Convert a string literal to its internal representation by removing the prefix (if
// any), processing escape codes if escapes? is true, and canonicalizing line endings to
// just \n.  Works for both one-line and multi-line strings because the lexer state
// transitions disallow CR and LF in one-line strings in the first place. bpos points to
// just after the start delimiter (", #", #r", """, etc) and epos points to the first
// character of the end delimiter.
define method decode-string
    (source-location :: <lexer-source-location>, bpos :: <integer>,
     epos :: <integer>, escapes? :: <boolean>)
 => (string :: <byte-string>, newline-count :: <integer>)
  local
    method warn (format-string, #rest format-args)
      note(<invalid-string-literal>,
           source-location: source-location,
           token-string: extract-string(source-location),
           detail: apply(format-to-string, format-string, format-args));
    end method,
    method whitespace-code? (c)
      c == $space-code
    end method,
    method find-line-break (seq, bpos, epos)
      if (bpos < epos)
        select (seq[bpos])
          $newline-code =>
            values(bpos, bpos + 1);
          $carriage-return-code =>
            if (bpos + 1 < epos & seq[bpos + 1] == $newline-code)
              values(bpos, bpos + 2)
            else
              values(bpos, bpos + 1)
            end;
          $tab-code =>
            warn("tab character at index %d; use \\t or spaces instead", bpos);
            find-line-break(seq, bpos + 1, epos);
          otherwise =>
            find-line-break(seq, bpos + 1, epos);
        end
      end
    end method,
    method remove-prefix (prefix, line)
      if (~prefix | empty?(prefix))
        line
      else
        every?(\==, line, prefix)
          | warn("each line must begin with the same whitespace prefix, got %=, want %=",
                 as(<string>, line), as(<string>, prefix));
        // In keeping with the C# spec, any prefix of the prefix is allowed, so it is
        // valid here if line.size < prefix.size.
        if (line.size < prefix.size)
          ""
        else
          copy-sequence(line, start: prefix.size)
        end
      end
    end method,
    // Can't use hex-escape-character because we don't know the correct offset from the
    // beginning of the literal due to using split/join.
    method parse-hex-escape (line, start) => (char, epos)
      let (code, epos)
        = parse-integer(source-location, line,
                        radix: 16, start: start, stop-at-non-digit?: #t);
      assert(epos <= line.size, "epos out of bounds: %d", epos);
      assert(line[epos] == as(<integer>, '>'),
             "hex escape must end with '>', got %=", line[epos]);
      if (code > $max-lexer-code)
        note(<character-code-too-large>,
             source-location: record-position-as-location
               (source-location.source-location-record,
                source-location.source-location-source-position),
             token-string: extract-string(source-location));
        values(0, epos)        // If forced, continue with NUL...
      else
        values(code, epos + 1)
      end
    end method,
    method process-escapes (line)
      let len = line.size;
      let new = make(<stretchy-vector>);
      iterate loop (pos = 0, escaped? = #f)
        if (pos >= len)
          as(<byte-vector>, new)
        else
          let code = line[pos];
          if (escaped?)
            let new-position
              = if (code == as(<integer>, '<'))
                  let (code, epos) = parse-hex-escape(line, pos + 1);
                  add!(new, code);
                  epos
                else
                  add!(new, as(<integer>, escape-character(as(<character>, code))));
                  pos + 1
                end;
            loop(new-position, #f)
          elseif (code == $escape-code)
            loop(pos + 1, #t)
          else
            add!(new, code);
            loop(pos + 1, #f)
          end
        end
      end iterate
    end method,
    method process-line (prefix, line)
      if (~empty?(line))
        if (prefix & ~empty?(prefix))
          line := remove-prefix(prefix, line);
        end;
        if (escapes? & member?($escape-code, line))
          line := process-escapes(line);
        end;
      end;
      line
    end method;
  let contents = source-location.source-location-record.contents;
  let parts = split(contents, find-line-break, start: bpos, end: epos);
  select (parts.size)
    1 =>
      values(as(<string>, process-line(#f, parts[0])), // e.g., """abc"""
             0);
    2 =>
      warn("multi-line strings must contain at least one line");
      values("", 1);
    otherwise =>
      let prefix = parts.last;
      every?(whitespace-code?, prefix)
        | warn("prefix must be all whitespace, got %=", as(<string>, prefix));
      every?(whitespace-code?, parts.first)
        // TODO: put the actual delimiter in the error message, in case it has more than
        // 3 double quotes.
        | warn("only whitespace may follow the open delimiter \"\"\" on the"
                 " same line, got %=", parts.first);
      let parts = map(curry(process-line, prefix),
                      copy-sequence(parts, start: 1, end: parts.size - 1));
      values(as(<string>, join(parts, make(<byte-vector>, size: 1, fill: $newline-code))),
             parts.size - 1);
  end select
end method decode-string;

// Make a <literal-token> when confronted with the foo: syntax.
//
define method make-keyword-symbol
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <keyword-syntax-symbol-fragment>)
  let sym = extract-symbol(source-location,
                           end: source-location.end-posn - 1);

  make(<keyword-syntax-symbol-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       value: as-fragment-value(sym));
end method make-keyword-symbol;


define constant $zero-code    :: <integer> = as(<integer>, '0');
define constant $nine-code    :: <integer> = as(<integer>, '9');
define constant $upper-a-code :: <integer> = as(<integer>, 'A');
define constant $lower-a-code :: <integer> = as(<integer>, 'a');
define constant $upper-f-code :: <integer> = as(<integer>, 'F');
define constant $lower-f-code :: <integer> = as(<integer>, 'f');
define constant $underscore_code :: <integer> = as(<integer>, '_');

// Parse and return an integer in the supplied radix.
//
define method parse-integer
    (source-location :: <lexer-source-location>, contents :: <byte-vector>,
     #key radix :: <integer> = 10,
          start :: <integer> = source-location.start-posn,
          end: finish :: <integer> = source-location.end-posn,
          stop-at-non-digit? = #f)
 => (res :: <abstract-integer>, end-pos :: <integer>)
  // We do our working in negative integers to avoid representation
  // overflow until absolutely necessary.
  local method repeat (posn :: <integer>, result :: <abstract-integer>)
                   => (final-result :: <abstract-integer>, end-pos :: <integer>)
          if (posn < finish)
            let digit :: <integer> = contents[posn];
            if ($zero-code <= digit & digit <= $nine-code)
              repeat(posn + 1,
                     generic-(generic*(result, radix),
                              digit - $zero-code));
            elseif ($upper-a-code <= digit & digit <= $upper-f-code)
              repeat(posn + 1,
                     generic-(generic*(result, radix),
                              10 + digit - $upper-a-code));
            elseif ($lower-a-code <= digit & digit <= $lower-f-code)
              repeat(posn + 1,
                     generic-(generic*(result, radix),
                              10 + digit - $lower-a-code));
            elseif (stop-at-non-digit?)
              values(result, posn)
            elseif (digit == $underscore_code) // Must follow stop-at-non-digit? check.
              // skip underscores
              repeat(posn + 1, result)
            else
              error("Bogus digit in integer: %=", as(<character>, digit));
            end if;
          else
            values(result, posn)
          end if
        end method repeat;
  let first = as(<character>, contents[start]);
  block ()
    if (first == '-')
      repeat(start + 1, 0)
    elseif (first == '+')
      let (int, epos) = repeat(start + 1, 0);
      values(genericnegative(int), epos)
    else
      let (int, epos) = repeat(start, 0);
      values(genericnegative(int), epos)
    end if
  exception (overflow :: <error>)
    note(<integer-too-large>,
         source-location:
            record-position-as-location
              (source-location.source-location-record,
               source-location.source-location-source-position),
         token-string: extract-string(source-location));
    // Return 0 if forced to continue...
    0;
  end;
end method parse-integer;

// Parse an integer and return a <literal-fragment> holding it.
//
define method parse-integer-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <abstract-integer-fragment>)
  let contents = source-location.source-location-record.contents;
  let posn = source-location.start-posn;
  let radix = 10;
  if (as(<character>, contents[posn]) == '#')
    let char = as(<character>, contents[posn + 1]);
    posn := posn + 2;
    radix := select (char)
               'b', 'B' => 2;
               'o', 'O' => 8;
               'x', 'X' => 16;
               otherwise =>
                 // theoretically unreachable
                 error("unexpected #%c integer literal prefix", char);
             end;
  end if;
  let int = parse-integer(source-location, contents, radix: radix, start: posn);
  if (int < $minimum-integer | int > $maximum-integer)
    make(<big-integer-fragment>,
         record: source-location.source-location-record,
         source-position: source-location.source-location-source-position,
         value:           as-fragment-value(int));
  else
    make(<integer-fragment>,
         record: source-location.source-location-record,
         source-position: source-location.source-location-source-position,
         value:           as-fragment-value(int));
  end;
end method parse-integer-literal;

// Return a <literal-token> holding the character token.
//
define method make-character-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <character-fragment>)
  let contents = source-location.source-location-record.contents;
  let posn = source-location.start-posn + 1;
  let char = as(<character>, contents[posn]);
  make(<character-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       value:
         as-fragment-value
           (if (char == '\\')
              let next-char = as(<character>, contents[posn + 1]);
              if (next-char == '<')
                hex-escape-character(source-location, posn + 2);
              else
                escape-character(next-char);
              end;
            else
              char;
            end));
end method make-character-literal;

define function increment-delimited-comment-start-count
    (lexer :: <lexer>)
  lexer.delimited-comment-start-count
    := lexer.delimited-comment-start-count + 1;
  // We tell the lexer to clear the previous accepting state, which was the binary
  // operator / prefixing the /*, so that if EOF is reached an error is signaled instead
  // of returning the wrong token.
  #"clear-previous"
end function;

define function increment-delimited-comment-end-count
    (lexer :: <lexer>)
  let count = lexer.delimited-comment-end-count + 1;
  lexer.delimited-comment-end-count := count;
  count == lexer.delimited-comment-start-count
end function;

define function increment-double-quote-start-count
    (lexer :: <lexer>)
  // If "" has been seen we can accept the empty string here.
  2 == (lexer.double-quote-start-count := lexer.double-quote-start-count + 1)
end function;

define function increment-double-quote-end-count
    (lexer :: <lexer>)
  let dq-end-count = (lexer.double-quote-end-count := lexer.double-quote-end-count + 1);
  dq-end-count == lexer.double-quote-start-count
    & dq-end-count ~== 2
    & #"accept"                 // Accept and do not try to match more.
end function;

// The lexer is transitioning away from a sequence of double quotes that could
// potentially be the end delimiter.
define function reset-double-quote-end-count
    (lexer :: <lexer>)
  let dq-start-count = lexer.double-quote-start-count;
  let dq-end-count   = lexer.double-quote-end-count;
  lexer.double-quote-end-count := 0;
  if (dq-start-count == dq-end-count | dq-start-count == 2)
    // #"use-previous" tells the lexer to use the previous accepting state immediately
    // rather than trying to greedily consume more. We want this both when we've seen a
    // full string with balanced delimiters and when the start delimiter is ``""``.
    // The latter must always be parsed as the empty string, not a start delimiter.
    #"use-previous"
  end
end function;

define function make-stringish-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (fragment :: <fragment>)
  let start :: <integer> = source-location.start-posn;
  let contents :: <byte-vector> = source-location.source-location-record.contents;
  let symbol? = contents[start] == $octothorp-code;
  let prefix-length = if (symbol?) 1 else 0 end;
  let string = extract-string-contents(prefix-length, #t, lexer, source-location);
  if (symbol?)
    make(<symbol-syntax-symbol-fragment>,
         record: source-location.source-location-record,
         source-position: source-location.source-location-source-position,
         value: as-fragment-value(as(<symbol>, string)))
  else
    make(<string-fragment>,
         record: source-location.source-location-record,
         source-position: source-location.source-location-source-position,
         value: as-fragment-value(string))
  end
end function;

define function make-raw-string-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (fragment :: <fragment>)
  let string = extract-string-contents(2, #f, lexer, source-location);
  make(<string-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       value: as-fragment-value(string))
end function;

define function extract-string-contents
    (prefix-length :: <integer>, escapes? :: <boolean>,
     lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (s :: <string>)
  let dq-start-count = lexer.double-quote-start-count;
  lexer.double-quote-start-count := 0;
  lexer.double-quote-end-count   := 0;
  let start = source-location.start-posn;
  let _end  = source-location.end-posn;
  // If the total length of the accepted string is 2 then it's the empty string. It's a
  // special case because the start/end delimiter counts may not be accurate when
  // fall-back to the previously accepting state ("") occurs.
  let shortest = 2 + prefix-length;
  if (_end - start == shortest)
    ""
  else
    let bpos = start + dq-start-count + prefix-length;
    let epos = _end - dq-start-count;
    decode-string(source-location, bpos, epos, escapes?)
  end
end function;

define method parse-ratio-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
  // => (res :: <fragment>)
  note(<ratios-not-supported>,
       source-location:
          record-position-as-location
            (source-location.source-location-record,
             source-location.source-location-source-position),
       token-string: extract-string(source-location));
end method parse-ratio-literal;

/*
define method parse-ratio-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <literal-token>;
  let slash
    = block (return)
        let contents = source-location.source-location-record.contents;
        for (posn from source-location.start-posn
               below source-location.end-posn)
          if (contents[posn] == as(<integer>, '/'))
            return(posn);
          end if;
        end for;
        error("No / in a ratio?");
        #f;
      end block;
  let numerator = parse-integer(source-location, end: slash);
  let denominator = parse-integer(source-location, start: slash + 1);
  make(<literal-token>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: $literal-token,
       literal: make(<literal-ratio>, value: ratio(numerator, denominator)));
end method parse-ratio-literal;
*/

// The default floating point precision, used for 'e' and floats without
// an exponent marker.

define constant <default-float> = <single-float>;

define constant $max-mantissa-digits = 18;

define method atof
    (string :: <byte-string>,
     #key start :: <integer> = 0,
          end: finish :: <integer> = string.size)
 => (class :: one-of(#f, #"single", #"double", #"extended"),
     value :: <float>)
  let class = #f;
  let posn = start;
  let sign = 1;
  let mantissa = 0;
  let scale = #f;
  let exponent-sign = 1;
  let exponent = 0;
  let exponent-shift = 0;
  let digits = 0;

  // Parse the optional sign.
  if (posn < finish)
    let char = string[posn];
    if (char == '-')
      posn := posn + 1;
      sign := -1;
    elseif (char == '+')
      posn := posn + 1;
    end if;
  end if;

  block (return)
    block (parse-exponent)
      // Parse the mantissa.
      while (posn < finish)
        let char = string[posn];
        posn := posn + 1;
        if (char >= '0' & char <= '9')
          if (digits < $max-mantissa-digits)
            let digit = as(<integer>, char) - as(<integer>, '0');
            mantissa := generic+(generic*(mantissa, 10), digit);
            if (scale)
              scale := generic*(scale, 10);
            end if;
          else
            // If we're after the decimal point, we can just ignore
            // the digit. If before, we have to remember that we've
            // been multiplied.
            if (~scale)
              exponent-shift := generic+(exponent-shift, 1);
            end;
          end;
          digits := digits + 1;
        elseif (char == '.')
          if (scale)
            error("multiple '.' characters in floating point literal");
          end if;
          scale := 1;
        elseif (char == 'e' | char == 'E')
          parse-exponent();
        elseif (char == 'd' | char == 'D')
          class := #"double";
          parse-exponent();
        elseif (char == 's' | char == 'S')
          class := #"single";
          parse-exponent();
        elseif (char == 'x' | char == 'X')
          class := #"extended";
          parse-exponent();
        elseif (char == '_')
          // Ignore, underscore is for readability only.
        else
          error("invalid %c character in floating point literal mantissa", char);
        end if;
      end while;
      return();
    end block; // parse-exponent

    // Parse the exponent.
    if (posn < finish)
      let char = string[posn];
      if (char == '-')
        exponent-sign := -1;
        posn := posn + 1;
      elseif (char == '+')
        posn := posn + 1;
      end if;

      while (posn < finish)
        let char = string[posn];
        if (char >= '0' & char <= '9')
          let digit = as(<integer>, char) - as(<integer>, '0');
          exponent := generic+(generic*(exponent, 10), digit);
        elseif (char == '_')
          // Ignore, underscore is for readability only.
        else
          error("invalid %c character in floating point literal exponent", char);
        end if;
        posn := posn + 1;
      end while;
    end if;
  end block;

  exponent := generic+(exponent, exponent-shift);

  // TODO: CORRECTNESS: Decide how to maintain precision and representation,
  // since we lose it here. (CMU used a ratio representation).
  // TODO: CORRECTNESS: Handle overflows reasonably gracefully.
  // TODO: CORRECTNESS: Note that we don't have extended floats
  //                    (i.e., <extended-float> == <double-float>)

  let (mantissa, base, scale)
    = select (class)
        #f          => values(as(<default-float>, mantissa), as(<single-float>, 10), as(<float>, scale | 1));
        #"single"   => values(as(<single-float>, mantissa), as(<single-float>, 10), as(<single-float>, scale | 1));
        #"double"   => values(as(<double-float>, mantissa), as(<double-float>, 10), as(<double-float>, scale | 1));
        #"extended" => values(as(<extended-float>, mantissa), as(<extended-float>, 10), as(<extended-float>, scale | 1));
      end;

  if (exponent = 0)
    values(class,
           generic/(generic*(sign, mantissa), scale))
  else
    let scaled-mantissa = generic/(generic*(sign, mantissa), scale);
    // NOTE: Floating point exponentiation loses precision for some
    // surprisingly small exponents so we'll use successive multiplications.
    //---*** NOTE: Revisit this as it may be costly w.r.t. consing and
    //---*** there must be a better way (rationals?).
    local method power-of-10 () => (power :: <float>)
            let iterate? = select (base by instance?)
                             <single-float> => exponent > 15;
                             // Yes, <double-float> exponentiation is never accurate!
                             <double-float> => #t;
                             //---*** NOTE: We don't have <extended-float>s yet ...
                             <extended-float> => #t;
                           end;
            if (iterate?)
              for (i from 1 below exponent)
                base := generic*(base, 10.0)
              end;
              base
            else
              generic^(base, exponent)
            end
          end method power-of-10;
    if (exponent-sign = 1)
      values(class,
             generic*(scaled-mantissa, power-of-10()))
    else
      values(class,
             generic/(scaled-mantissa, power-of-10()))
    end
  end
end method atof;

define method parse-fp-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <float-fragment>)
  let (class, value) = atof(extract-string(source-location));

  // TODO: Decide how to maintain precision and representation,
  // since we lose it here. (CMU used a ratio representation).
  make(<float-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       value: as-fragment-float-value(class, value));
end method parse-fp-literal;

define constant $hash-data-start-delimiters :: <byte-string> = "\"{[(|`'";
define constant $hash-data-end-delimiters   :: <byte-string> = "\"}])|`'";
define constant $escape-code = as(<integer>, '\\');

// The lexer has just seen the second colon in "#:foo:{bar}" or
// "#:foo:bar" and now tries to parse the string "bar" and emit the
// parse tree for foo-parser("bar").
//
// TODO: this can now mostly be replaced with <conditionally-accepting-state>s in the
// state machine; essentially just make the fragments. delimiter? is also only used by
// this function.
define method make-hash-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
 => (res :: <fragment>)
  let name-string
    = extract-string(source-location,
                     start: source-location.start-posn + 2,
                     end:   source-location.end-posn - 1);
  block ()
    let contents :: <byte-vector> = lexer.lexer-source.contents;
    let length :: <integer> = contents.size;
    let data-start :: <integer> = lexer.lexer-position;
    let delimiter = contents[data-start];
    let delimiter-index
      = position($hash-data-start-delimiters, as(<character>, delimiter));
    let data :: <byte-string> = "";
    if (delimiter-index)
      // Read until the matching delimiter
      let start-delimiter = delimiter;
      let end-delimiter
        = as(<integer>, $hash-data-end-delimiters[delimiter-index]);
      iterate loop (i = data-start + 1, prev-char = 0)
        if (i >= length)
          note(<unterminated-parser-expansion>,
               source-location:
                 record-position-as-location
                 (source-location.source-location-record,
                  source-location.source-location-source-position),
               token-string: extract-string(source-location));
        else
          let char :: <integer> = contents[i];
          if (char == end-delimiter & (prev-char ~== $escape-code))
            data := extract-string(source-location, start: data-start + 1, end: i);
            lexer.lexer-position := i + 1;
          else
            if (char == $newline-code)
              lexer.lexer-line-number := lexer.lexer-line-number + 1;
              lexer.lexer-line-start-position := i;
            end;
            loop(i + 1, char);
          end;
        end;
      end iterate;
    else
      // Read until whitespace or EOF
      let i :: <integer> = data-start;
      let char :: <integer> = 0;
      while (i < contents.size & ~delimiter-code?((char := contents[i])))
        i := i + 1;
      end;
      data := extract-string(source-location, start: data-start, end: i);
      lexer.lexer-position := i;
    end;
    let module = *current-module*;
    let parser-symbol = as(<symbol>, concatenate(name-string, "-parser"));
    let parser-name
      = make(<variable-name-fragment>,
             record: source-location.source-location-record,
             source-position: source-location.source-location-source-position,
             name: parser-symbol,
             kind: syntax-for-name(module, parser-symbol),
             module: module);
    let data-string
      = make(<string-fragment>,
             record: source-location.source-location-record,
             source-position: source-location.source-location-source-position,
             value: as-fragment-value(data));
    let call
      = make(<prefix-call-fragment>,
             record: fragment-record(parser-name),
             source-position: position-between(parser-name, parser-name),
             function: parser-name,
             arguments: list(data-string));
    call
  end;
end method make-hash-literal;

define constant $comma-code = as(<integer>, ',');
define constant $semicolon-code = as(<integer>, ';');
define constant $lparen-code = as(<integer>, '(');
define constant $rparen-code = as(<integer>, ')');
define constant $lbracket-code = as(<integer>, '[');
define constant $rbracket-code = as(<integer>, ']');
define constant $lbrace-code = as(<integer>, '{');
define constant $rbrace-code = as(<integer>, '}');

define function delimiter-code?
    (code :: <integer>) => (whitespace? :: <boolean>)
  select (code)
    $space-code, $newline-code, $tab-code => #t;
    $comma-code, $semicolon-code          => #t;
    $lparen-code, $rparen-code            => #t;
    $lbracket-code, $rbracket-code        => #t;
    $lbrace-code, $rbrace-code            => #t;
    otherwise => #f;
  end;
end function delimiter-code?;


// Conditional compilation stuff.

/*
define class <conditional-state> (<object>)
  slot active? :: <boolean>,
    required-init-keyword: active:;
  slot do-else? :: <boolean>,
    required-init-keyword: do-else:;
  slot seen-else? :: <boolean>,
    init-value: #f;
  slot old-state :: false-or(<conditional-state>),
    required-init-keyword: old-state:;
end class <conditional-state>;

define sealed domain make (singleton(<conditional-state>));
define sealed domain initialize (<conditional-state>);


define method active? (state == #f) => res :: <boolean>;
  #t;
end method active?;


define method parse-error (token :: <fragment>) => ();
  compiler-fatal-error
    ("syntax error in feature condition at or before %=", token);
end method parse-error;


define method parse-feature-term (lexer :: <lexer>) => res :: <boolean>;
  let token = internal-get-token(lexer);
  let kind = token.token-kind;
  if (kind == $left-paren-token)
    parse-feature-expr(lexer);
  elseif (kind == $tilde-token)
    ~parse-feature-term(lexer);
  elseif (kind >= $define-token & kind <= $quoted-name-token)
    feature-present?(token.token-symbol);
  else
    parse-error(token);
  end if;
end method parse-feature-term;


define method parse-feature-expr (lexer :: <lexer>) => res :: <boolean>;
  block (return)
    let res = parse-feature-term(lexer);
    while (#t)
      let token = internal-get-token(lexer);
      let kind = token.token-kind;
      if (kind == $right-paren-token)
        return(res);
      elseif (kind == $other-binary-operator-token)
        select (token.token-symbol)
          #"&" =>
            if (~parse-feature-term(lexer))
              res := #f;
            end if;
          #"|" =>
            if (parse-feature-term(lexer))
              res := #t;
            end if;
          otherwise =>
            parse-error(token);
        end select;
      else
        parse-error(token);
      end if;
    end while;
  end block;
end method parse-feature-expr;

define method parse-conditional (lexer :: <lexer>) => res :: <boolean>;
  let token = internal-get-token(lexer);
  unless (token.token-kind == $left-paren-token)
    parse-error(token);
  end unless;
  parse-feature-expr(lexer);
end method parse-conditional;
*/

// A very simple heuristic is used here - we just skip forward until we
// come across the next flush-left character that doesn't look like "end".

// TODO: CORRECTNESS: Multiplatform newline sequence handling.

define constant $space-code = as(<integer>, ' ');
define constant $carriage-return-code = as(<integer>, '\r');
define constant $newline-code = as(<integer>, '\n');
define constant $tab-code = as(<integer>, '\t');

define constant $lower-e-code = as(<integer>, 'e');
define constant $upper-e-code = as(<integer>, 'E');
define constant $lower-n-code = as(<integer>, 'n');
define constant $upper-n-code = as(<integer>, 'N');
define constant $lower-d-code = as(<integer>, 'd');
define constant $upper-d-code = as(<integer>, 'D');

define inline function at-newline?
    (contents :: <byte-vector>, posn :: <integer>)
 => (newline? :: <boolean>, posn-after :: <integer>)
  if (contents[posn] == $newline-code)
    values(#t, posn + 1);
  else
    values(#f, posn);
  end;
end function at-newline?;

define inline function at-whitespace?
    (contents :: <byte-vector>, posn :: <integer>, length :: <integer>)
 => (whitespace? :: <boolean>)
  posn < length
    & begin
        let c = contents[posn];
        c == $space-code | c == $newline-code | c == $tab-code
      end;
end function at-whitespace?;

// TODO: CORRECTNESS: Allow for names prefixed end like "ending ... end".

define inline function at-end-word?
    (contents :: <byte-vector>, posn :: <integer>, length :: <integer>)
 => (end-word? :: <boolean>)
  posn + 3 <= length
    & begin
        let e = contents[posn];
        let n = contents[posn + 1];
        let d = contents[posn + 2];
        (e == $lower-e-code | e == $upper-e-code)
        & (n == $lower-n-code | n == $upper-n-code)
        & (d == $lower-d-code | d == $upper-d-code)
      end;
end function at-end-word?;

// Scan until we find anything on the left margin that is not "end".
define method skip-to-next-top-level-form
    (lexer :: <lexer>) => ()
  let contents = lexer.lexer-source.contents;
  let length = contents.size;
  local method walk (i :: <integer>)
    if (i < length)
      let (newline?, start) = at-newline?(contents, i);
      if (newline?)
        lexer.lexer-line-start-position := start;
        lexer.lexer-line-number := lexer.lexer-line-number + 1;
        if (~at-whitespace?(contents, start, length)
              & ~at-end-word?(contents, start, length))
          start
        else
          walk(start);
        end;
      else
        walk(i + 1);
      end;
    else
      length
    end;
  end;
  lexer.lexer-position := walk(lexer.lexer-position);
end method skip-to-next-top-level-form;
