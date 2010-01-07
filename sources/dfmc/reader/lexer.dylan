Module:   dfmc-reader
Synopsis: The state transition interpreter for the lexer, plus support
          routines for further parsing and token construction.
Author:   CMU, adapted by Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Derived from CMU code.
// 
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.

define constant $max-lexer-code :: <integer> = 255;


// state machine.

// <state> -- internal.
//
// A particular state in the state machine.
// 
define class <state> (<object>)
  //
  // The name of this state, a symbol.  Not really used once the state
  // machine is built, but we keep it around for debugging purposes.
  slot name :: <symbol>, required-init-keyword: name:;
  //
  // The acceptance result if this state is an accepting state, or #f
  // if it is not.  Symbols are used for magic interal stuff that never
  // makes it out of the lexer (e.g. whitespace), classes for simple
  // tokens that don't need any extra parsing, and functions for more
  // complex tokens.
  constant slot result :: type-union(<false>, <symbol>, <class>, <function>),
    required-init-keyword: result:;
  //
  // Either #f or a vector of next-states indexed by character code.
  // During construction, vector elements are either state names or #f.
  // After construction, the state names are replaced by the actual
  // state objects.
  constant slot transitions :: false-or(<simple-object-vector>),
    required-init-keyword: transitions:;
end class <state>;

define sealed domain make (singleton(<state>));
define sealed domain initialize (<state>);

define method print-object (state :: <state>, stream :: <stream>) => ();
  pprint-fields(state, stream, name: state.name);
end method print-object;


define method add-transition
    (table :: <simple-object-vector>,
     on :: type-union(<integer>, <character>, <byte-string>),
     new-state :: <symbol>)
    => ();
  //
  // Make as many entries are necessary to represent the transitions
  // from on to new-state.  On can be either an integer, a character,
  // or a byte-string.  If a byte-string, then it supports ranges
  // as in a-z.
  //
  // We also check to see if this entry classes with any earlier
  // entries.  If so, it means someone messed up editing the
  // state machine.
  // 
  select (on by instance?)
    <integer> =>
      if (table[on])
	error("input %= transitions to both %= and %=",
	      as(<character>, on), table[on], new-state);
      else
	table[on] := new-state;
      end if;
    <character> =>
      add-transition(table, as(<integer>, on), new-state);
    <byte-string> =>
      let last = #f;
      let range = #f;
      for (char :: <byte-character> in on)
	if (range)
	  if (last)
	    for (i :: <integer> 
                   from as(<integer>, last) + 1 to as(<integer>, char))
	      add-transition(table, i, new-state);
	    end for;
	    last := #f;
	  else
	    add-transition(table, as(<integer>, '-'), new-state);
	    add-transition(table, as(<integer>, char), new-state);
	    last := char;
	  end if;
	  range := #f;
	elseif (char == '-')
	  range := #t;
	else 
	  add-transition(table, as(<integer>, char), new-state);
	  last := char;
	end if;
      end for;
  end select;
end method add-transition;

define method state
    (name :: <symbol>,
     result :: type-union(<false>, <symbol>, <class>, <function>),
     #rest transitions)
  //
  // Utility function for making states.  We expand the sequence
  // of transitions into a transition table and make the state object.
  //
  let table = size(transitions) > 0
    & make(<vector>, size: $max-lexer-code + 1, fill: #f);
  for (transition in transitions)
    add-transition(table, head(transition), tail(transition));
  end for;
  make(<state>,
       name: name,
       result: result,
       transitions: table);
end method state;


define method compile-state-machine (#rest states)
    => start-state :: <state>;
  //
  // make a hash table mapping state names to states.
  // 
  let state-table = make(<table>);
  for (state in states)
    if (element(state-table, state.name, default: #f))
      error("State %= multiply defined.", state.name);
    else
      state-table[state.name] := state;
    end if;
  end for;
  //
  // Now that we have a table mapping state names to states, change the
  // entries in the transition tables to refer to the new state
  // object themselves instead of just to the new state name.
  // 
  for (state in states)
    let table = state.transitions;
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
  // $Initial-State to hold.
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
						    

// lexer

// <lexer> -- exported.
//
// An object holding the current lexer state.
//
define class <lexer> (<tokenizer>)
  //
  // The compilation record we are currently tokenizing.
  constant slot source :: <compilation-record>, required-init-keyword: source:;
  //
  // The position we are currently at in the source file.
  slot posn :: <integer>, required-init-keyword: start-posn:;
  //
  // The line number we are currently working on.
  slot line :: <integer>, required-init-keyword: start-line:;
  //
  // The position that this line started at.
  slot line-start :: <integer>, required-init-keyword: start-posn:;
  //
  // A list of tokens that have been unread.
  // slot pushed-tokens :: <list>, init-value: #();
  //
  // slot conditional-state :: false-or(<conditional-state>), init-value: #f;
  //
  // The last token read.
  slot last-token :: <object> = #f;
  // Tame source location object.
  constant slot tame-source-location :: <lexer-source-location>
    = make(<lexer-source-location>, 
           source-record: #f, 
           start-posn: 0, end-posn: 0);
end class <lexer>;

define sealed domain make (singleton(<lexer>));
define sealed domain initialize (<lexer>);

define method print-object (lexer :: <lexer>, stream :: <stream>) => ();
  pprint-fields(lexer, stream,
		source: lexer.source.compilation-record-source-record,
		posn: lexer.posn,
		line: lexer.line,
		column: lexer.posn - lexer.line-start + 1);
end method print-object;

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
end function;

// skip-multi-line-comment -- internal.
//
// Skip a multi-line comment, taking into account nested comments.
//
// Basically, we just implement a state machine via tail recursive local
// methods.
//
define method skip-multi-line-comment (lexer :: <lexer>,
				       start :: <integer>)
    => result :: false-or(<integer>);
  let contents = lexer.source.contents;
  let length = contents.size;
  local
    //
    // Utility function that checks to make sure we haven't run off the
    // end before calling the supplied function.
    //
    method next (func :: <function>, posn :: <integer>, depth :: <integer>)
      if (posn < length)
	func(as(<character>, contents[posn]), posn + 1, depth);
      else
	#f;
      end if;
    end next,
    //
    // Seen nothing of interest.  Look for the start of any of /*, //, or */
    //
    method seen-nothing (char :: <character>, posn :: <integer>,
			 depth :: <integer>)
      if (char == '/')
	next(seen-slash, posn, depth);
      elseif (char == '*')
	next(seen-star, posn, depth);
      elseif (char == '\n')
	lexer.line := lexer.line + 1;
	lexer.line-start := posn;
	next(seen-nothing, posn, depth)
      else
	next(seen-nothing, posn, depth);
      end if;
    end seen-nothing,
    //
    // Okay, we've seen a slash.  Look to see if it was /*, //, or just a
    // random slash in the source code.
    //
    method seen-slash (char :: <character>, posn :: <integer>,
		       depth :: <integer>)
      if (char == '/')
	next(seen-slash-slash, posn, depth);
      elseif (char == '*')
	next(seen-nothing, posn, depth + 1);
      elseif (char == '\n')
	lexer.line := lexer.line + 1;
	lexer.line-start := posn;
	next(seen-nothing, posn, depth)
      else
	next(seen-nothing, posn, depth);
      end if;
    end seen-slash,
    //
    // Okay, we've seen a star.  Look to see if it was */ or a random star.
    // We also have to check to see if this next character is another star,
    // because if so, it might be the start of a */.
    //
    method seen-star (char :: <character>, posn :: <integer>,
		      depth :: <integer>)
      if (char == '/')
	if (depth == 1)
	  posn;
	else
	  next(seen-nothing, posn, depth - 1);
	end if;
      elseif (char == '*')
	next(seen-star, posn, depth);
      elseif (char == '\n')
	lexer.line := lexer.line + 1;
	lexer.line-start := posn;
	next(seen-nothing, posn, depth)
      else
	next(seen-nothing, posn, depth);
      end if;
    end seen-star,
    //
    // We've seen a //, so skip until the end of the line.
    //
    method seen-slash-slash (char :: <character>, posn :: <integer>,
			     depth :: <integer>)
      if (char == '\n')
	lexer.line := lexer.line + 1;
	lexer.line-start := posn;
	next(seen-nothing, posn, depth);
      else
	next(seen-slash-slash, posn, depth);
      end if;
    end seen-slash-slash;
  //
  // Start out not having seen anything.
  //
  next(seen-nothing, start, 1);
end method;

// internal-get-token -- internal.
//
// Tokenize the next token and return it.
//

define macro fragment-builder
  { fragment-builder(?:name) }
    => { method (lexer, source-location :: <lexer-source-location>)
           make(?name, 
                record: 
                  source-location.source-location-record,
                source-position: 
                  source-location.source-location-source-position)
         end }
end macro;

// TODO: Lose the hand inlining of maybe-done when the compiler's smarter.

define method get-token (lexer :: <lexer>) => res :: <fragment>;
  //
  // Basically, just record where we are starting, and keep
  // advancing the state machine until there are no more possible
  // advances.  We don't stop at the first accepting state we find,
  // because the longest token is supposed to take precedence.  We
  // just note where the last accepting state we came across was,
  // and then when the state machine jams, we just use that latest
  // accepting state's result.
  // 
  let contents :: <byte-vector> = lexer.source.contents;
  let length :: <integer> = contents.size;
  let unexpected-eof :: <boolean> = #f;
  let saved-line :: false-or(<integer>) = #f;
  let saved-line-start :: false-or(<integer>) = #f;

  let result-kind = #f;
  let result-start = lexer.posn;
  let result-end = #f;

 without-bounds-checks

  local
    method repeat
        (state :: <state>, posn :: <integer>
           /* , result-kind, result-start, result-end */)
      if (state.result)
	//
	// It is an accepting state, so record the result and where
	// it ended.
	// 
	result-kind := state.result;
	result-end := posn;
      end if;
      //
      // Try advancing the state machine once more if possible.
      // 
      if (posn < length)
	let table = state.transitions;
	let char :: <integer> = contents[posn];
	let new-state 
          = if (table /* & char < $max-lexer-code + 1 */)
              let table :: <simple-object-vector> = table;
              vector-element(table, char);
            end;
	if (new-state)
          let new-state :: <state> = new-state;
	  repeat
            (new-state, posn + 1
               /* , result-kind, result-start, result-end */);
	else
          /*
	  maybe-done
            (contents, length, result-kind, result-start, result-end);
          */

      //
      // maybe-done is called when the state machine cannot be
      // advanced any further.  It just checks to see if we really
      // are done or not.
      //
      if (instance?(result-kind, <symbol>))
      // if (object-class(result-kind) == <symbol>)
	//
	// The result-kind is a symbol if this is one of the magic
	// accepting states.  Instead of returning some token, we do
	// some special processing depending on exactly what symbol
	// it is, and then start the state machine over at the
	// initial state.
	//
	select (result-kind)
	  #"whitespace" =>
	    #f;
	  #"newline" =>
            let result-end :: <integer> = result-end;
	    lexer.line := lexer.line + 1;
	    lexer.line-start := result-end;
	  #"end-of-line-comment" =>
	    for (i :: <integer> from result-end below length,
		 until: (contents[i] == as(<integer>, '\n')))
	    finally
	      result-end := i;
	    end for;
	  #"multi-line-comment" =>
	    saved-line := lexer.line;
	    saved-line-start := lexer.line-start;
	    result-end := skip-multi-line-comment(lexer, result-end);
            if (result-end) 
              saved-line := #f;
              saved-line-start := #f;
            else
	      unexpected-eof := #t;
            end;
	end select;
	result-kind := #f;
	if (result-end)
          // let result-start :: <integer> = result-end;
          // let result-end = #f;
          result-start := result-end;
          result-end := #f;
          let result-start :: <integer> = result-start;
	  repeat
            ($Initial-State, result-start 
               /* , result-kind, result-start, result-end */);
	else
	  values(posn, result-kind, result-start, result-end)
	end if;
      else
	values(posn, result-kind, result-start, result-end)
      end if;
	end if;
      else
        /*
	maybe-done
          (contents, length, result-kind, result-start, result-end);
        */
      //
      // maybe-done is called when the state machine cannot be
      // advanced any further.  It just checks to see if we really
      // are done or not.
      //
      if (instance?(result-kind, <symbol>))
      // if (object-class(result-kind) == <symbol>)
	//
	// The result-kind is a symbol if this is one of the magic
	// accepting states.  Instead of returning some token, we do
	// some special processing depending on exactly what symbol
	// it is, and then start the state machine over at the
	// initial state.
	//
	select (result-kind)
	  #"whitespace" =>
	    #f;
	  #"newline" =>
            let result-end :: <integer> = result-end;
	    lexer.line := lexer.line + 1;
	    lexer.line-start := result-end;
	  #"end-of-line-comment" =>
	    for (i :: <integer> from result-end below length,
		 until: (contents[i] == as(<integer>, '\n')))
	    finally
	      result-end := i;
	    end for;
	  #"multi-line-comment" =>
	    result-end := skip-multi-line-comment(lexer, result-end);
            if (~result-end) unexpected-eof := #t end;
	end select;
	result-kind := #f;
	if (result-end)
          // let result-start :: <integer> = result-end;
          // let result-end = #f;
          result-start := result-end;
          result-end := #f;
          let result-start :: <integer> = result-start;
	  repeat
            ($Initial-State, result-start 
               /* , result-kind, result-start, result-end */);
	else
	  values(posn, result-kind, result-start, result-end)
	end if;
      else
	values(posn, result-kind, result-start, result-end)
      end if;
      end if;
    end method repeat;
  let (posn, result-kind, result-start, result-end)
    = repeat
        ($Initial-State, lexer.posn
           /* , #f, lexer.posn, #f */);
  if (~result-kind)
    //
    // If result-kind is #f, that means we didn't find an accepting
    // state.  Check to see if that means we are at the end or hit
    // an error.
    // 
    if (result-start == length)
      result-kind := fragment-builder(<eof-marker>);
      result-end := result-start;
    elseif (unexpected-eof | posn == length)
      result-kind := #f;
      result-end := length;
      unexpected-eof := #t;
    else
      result-kind := #f;
      result-end := result-start + 1;
    end if;
  end if;
  //
  // Save the current token's end position so that the next token
  // starts here.
  //
  let result-end :: <integer> = result-end;
  lexer.posn := result-end;
  //
  // Make a source location for the current token.
  // 
  let effective-line :: <integer> = saved-line | lexer.line;
  let effective-line-start :: <integer> = saved-line-start | lexer.line-start;
  let source-location
    = make-lexer-source-location
        (lexer, lexer.source,
	 result-start, effective-line,
	   result-start - effective-line-start,
	 result-end, lexer.line, result-end - lexer.line-start);
  //
  // And finally, make and return the actual token.
  // 
  if (result-kind)
    do-process-token(result-kind, lexer, source-location);
  else
    if (unexpected-eof)
      invalid-end-of-input(source-location);
    else
      invalid-token(source-location);
    end;
  end if;
 end;
end method get-token;

// This indirection is only here for profiling perposes.

define inline function do-process-token (f, lexer :: <lexer>, source-location)
  lexer.last-token := f(lexer, source-location);
end function;

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

// make-binary-operator, make-tilde, make-minus, make-equal, make-double-equal
//   -- internal.
//
// Make various kinds of operators.
// 
define method make-binary-operator
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <binary-operator-fragment>;
  make(<binary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: $binary-operator-only-token,
       name: extract-symbol(source-location),
       module: *Current-Module*);
end method make-binary-operator;
//
define method make-tilde
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <unary-operator-fragment>;
  make(<unary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       name: #"~",
       module: *Current-Module*);
end method make-tilde;
//
define method make-minus
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <unary-and-binary-operator-fragment>;
  make(<unary-and-binary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: $unary-and-binary-operator-token,
       name: #"-",
       module: *Current-Module*);
end method make-minus;
//
define method make-equal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <equal-fragment>;
  make(<equal-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position);
end method make-equal;
//
define method make-double-equal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <binary-operator-fragment>;
  make(<binary-operator-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: $equal-equal-token,
       name: #"==",
       module: *Current-Module*);
end method make-double-equal;


// make-quoted-name -- internal.
//
// Make a <quoted-name-token> for \-quoted operator.
// 
define method make-quoted-name
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <variable-name-fragment>;
  make(<escaped-name-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       name: extract-symbol(source-location,
			    start: source-location.start-posn + 1),
       module: *Current-Module*);
end method make-quoted-name;

// make-identifier -- internal.
//
// Extract the name from the source location, figure out what kind of word it
// is, and make it.
// 
define method make-identifier
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <variable-name-fragment>;
  let name = extract-symbol(source-location);
  let module = *Current-Module*;
  make(<variable-name-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       kind: syntax-for-name(module, name),
       name: name,
       module: module);
end method make-identifier;

// make-history-name -- internal.
//
// We allow extended $[0-9]+ syntax for getting at history bindings.
// For now, these aren't distinguished syntactically from other names.
// 
define method make-history-name
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <variable-name-fragment>;
  make-identifier(lexer, source-location);
end method make-history-name;


// make-constrainted-name -- internal.
//
// Make a constrained name.
// 
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
  let last = last-token(lexer);
  if (~explicit-name?
        | instance?(last, <query-fragment>)
        | instance?(last, <query-query-fragment>)
        | instance?(last, <escaped-substitution-fragment>))
    make(<constrained-name-fragment>,
         record: source-location.source-location-record,
         source-position: source-location.source-location-source-position,
         kind: $constrained-name-token,
         name: name,
         constraint: constraint);
  else
    make-qualified-name(lexer, source-location);
  end;
end method;

// make-qualified-name -- internal.
//
// Make a qualified name.
// 
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
end method;

// escape-character -- internal.
//
// Return the real character that corresponds to the \-quoted
// character in a string or character literal.
//
define method escape-character
    (char :: <character>) => escaped-char :: <character>;
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
 => (char :: <character>)
  let code 
    = parse-integer
        (source-location, 
           start:              start,
           stop-at-non-digit?: #t,
           radix:               16);
  if (code > $max-lexer-code)
    note(<character-code-too-large>,
         source-location: 
            record-position-as-location
              (source-location.source-location-record,
               source-location.source-location-source-position),
         token-string: extract-string(source-location));
    // If forced, continue with nul...
    as(<character>, 0);
  else
    as(<character>, code);
  end;
end method;

// decode-string -- internal.
//
// Like extract string, except process escape characters.  Also, we
// default to starting one character in from either end, under the
// assumption that the string will be surrounded by quotes.
//
define method decode-string
    (source-location :: <lexer-source-location>,
     #key start :: <integer> = source-location.start-posn + 1,
     end: finish :: <integer> = source-location.end-posn - 1)
    => result :: <byte-string>;
  let contents = source-location.source-location-record.contents;
  local method skip-hex-escape (contents, posn)
    if (contents[posn] == as(<integer>, '>'))
      posn + 1
    else
      skip-hex-escape(contents, posn + 1)
    end;
  end method;
  let length 
    = begin
        local method repeat (posn, result)
          if (posn < finish)
            if (contents[posn] == as(<integer>, '\\'))
              if (contents[posn + 1] == as(<integer>, '<'))
                repeat (skip-hex-escape(contents, posn), result + 1);
              else
                repeat (posn + 2, result + 1);
              end;
            else
              repeat (posn + 1, result + 1);
            end if;
          else
            result;
          end if;
        end method repeat;
	repeat(start, 0);
      end;
  let result = make(<string>, size: length);
  local method repeat (src, dst)
	  if (dst < length)
	    if (contents[src] == as(<integer>, '\\'))
              let next = contents[src + 1];
              if (next == as(<integer>, '<'))
                result[dst] := hex-escape-character(source-location, src + 2);
                repeat(skip-hex-escape(contents, src), dst + 1);
              else
                result[dst] := escape-character(as(<character>, next));
                repeat(src + 2, dst + 1);
              end;
	    else
	      result[dst] := as(<character>, contents[src]);
	      repeat(src + 1, dst + 1);
	    end if;
	  end if;
	end method repeat;
  repeat(start, 0);
  result;
end method decode-string;
			 
// make-quoted-symbol -- internal.
//
// Make a <literal-token> when confronted with the #"foo" syntax.
//
define method make-quoted-symbol
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <symbol-syntax-symbol-fragment>;
  let sym = as(<symbol>,
	       decode-string(source-location,
			     start: source-location.start-posn + 2));
  make(<symbol-syntax-symbol-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       value: as-fragment-value(sym));
end method make-quoted-symbol;

// make-keyword-symbol -- internal.
//
// Make a <literal-token> when confronted with the foo: syntax.
// 
define method make-keyword-symbol
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <keyword-syntax-symbol-fragment>;
  let sym = extract-symbol(source-location,
			   end: source-location.end-posn - 1);

  make(<keyword-syntax-symbol-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       value: as-fragment-value(sym));
end method make-keyword-symbol;
		    
// parse-integer -- internal.
//
// Parse and return an integer in the supplied radix.
// 

define constant $zero-code    :: <integer> = as(<integer>, '0');
define constant $nine-code    :: <integer> = as(<integer>, '9');
define constant $upper-a-code :: <integer> = as(<integer>, 'A');
define constant $lower-a-code :: <integer> = as(<integer>, 'a');
define constant $upper-f-code :: <integer> = as(<integer>, 'F');
define constant $lower-f-code :: <integer> = as(<integer>, 'f');

define method parse-integer
    (source-location :: <lexer-source-location>,
     #key radix :: <integer> = 10,
          start :: <integer> = source-location.start-posn,
          end: finish :: <integer> = source-location.end-posn,
          stop-at-non-digit? = #f)
    => res :: <extended-integer>;
  let contents :: <byte-vector>
    = source-location.source-location-record.contents;
  // We do our working in negative integers to avoid representation 
  // overflow until absolutely necessary.
  local method repeat 
                   (posn :: <integer>, result :: <abstract-integer>)
                => (final-result :: <abstract-integer>)
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
              result
            else
	      error("Bogus digit in integer: %=", as(<character>, digit));
	    end if;
	  else
	    result;
	  end if;
	end method repeat;
  let first = as(<character>, contents[start]);
  block ()
    if (first == '-')
      repeat(start + 1, 0);
    elseif (first == '+')
      genericnegative(repeat(start + 1, 0));
    else
      genericnegative(repeat(start, 0));
    end if;
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

// parse-integer-literal -- all internal.
//
// Parse an integer and return a <literal-token> holding it.
// 

define program-warning <integer-literal-too-large>
  slot condition-integer-string,
    required-init-keyword: integer-string:;
  format-string 
    "The integer literal %s is too large to be a small integer.";
  format-arguments integer-string;
end program-warning;

define method parse-integer-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <abstract-integer-fragment>;
  let contents = source-location.source-location-record.contents;
  let posn = source-location.start-posn;
  let extended = #f;
  let radix = 10;

  if (as(<character>, contents[posn]) == '#')
    posn := posn + 1;
    let char = as(<character>, contents[posn]);
    if (char == 'e' | char == 'E')
      posn := posn + 1;
      char := as(<character>, contents[posn]);
      extended := #t;
    end if;
    if (char == 'b' | char == 'B')
      posn := posn + 1;
      radix := 2;
    elseif (char == 'o' | char == 'O')
      posn := posn + 1;
      radix := 8;
    elseif (char == 'x' | char == 'X')
      posn := posn + 1;
      radix := 16;
    end if;
  end if;
  
  let int = parse-integer(source-location, radix: radix, start: posn);

  if (~extended &
	(int < runtime-$minimum-integer
	   | int > runtime-$maximum-integer))
//    note(<integer-literal-too-large>,
//         source-location: record-position-as-location
//	                   (source-location.source-location-record,
//			    source-location.source-location-source-position),
//         integer-string:  extract-string(source-location));
    extended := #t;
  end if;

  if (extended)
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

// make-character-literal -- internal.
//
// Return a <literal-token> holding the character token.
// 
define method make-character-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <character-fragment>;
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

// make-string-literal -- internal.
//
// Should be obvious by now.
//
define method make-string-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <string-fragment>;
  make(<string-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       // kind: $string-token,
       value: as-fragment-value(decode-string(source-location)));
end method make-string-literal;

// parse-ratio-literal -- internal.
// 
define method parse-ratio-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    // => res :: <fragment>;
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

define method atof (string :: <byte-string>,
		    #key start :: <integer> = 0,
		         end: finish :: <integer> = string.size)
    => (class :: one-of(#f, #"single", #"double", #"extended"),
	value :: <float>);
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
	    error("bogus float.");
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
	else
	  error("bogus float.");
	end if;
      end while;
      return();
    end block;

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
	else
	  error("bogus float");
	end if;
        posn := posn + 1;
      end while;
    end if;
  end block;

  exponent := generic+(exponent, exponent-shift);

  // TODO: CORRECTNESS: Decide how to maintain precision and representation,
  // since we lose it here. (CMU used a ratio representation).
  // TODO: CORRECTNESS: Handle overflows reasonably gracefully.
  // TODO: CORRECTNESS: Note that we don't have extended floats.

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
    // suprisingly small exponents so we'll use successive multiplications.
    //---*** NOTE: Revisit this as it may be costly w.r.t. consing and
    //---*** there must be a better way (rationals?).
    local method power-of-10 () => (power :: <float>)
	    let iterate? = select (base by instance?)
			     <single-float> => exponent > 15;
			     // Yes, <double-float> exponentation is never accurate!
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

// parse-fp-literal -- internal.
// 
define method parse-fp-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <number-fragment>;
  let (class, value) = atof(extract-string(source-location));

  // TODO: Decide how to maintain precision and representation,
  // since we lose it here. (CMU used a ratio representation).
  make(<number-fragment>,
       record: source-location.source-location-record,
       source-position: source-location.source-location-source-position,
       value: as-fragment-float-value(class, value));
end method parse-fp-literal;

// make-hash-word
//

define method make-hash-word
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <fragment>;
  let name
    = extract-symbol(source-location, 
                     start: source-location.start-posn + 1);
  let core-class
    = select (name)
        #"t"        => <true-fragment>;
        #"f"        => <false-fragment>;
        #"next"     => <hash-next-fragment>;
        #"rest"     => <hash-rest-fragment>;
        #"key"      => <hash-key-fragment>;
        #"all-keys" => <hash-all-keys-fragment>;
        otherwise   => #f;
      end;
  if (core-class)
    make(core-class,
         record: source-location.source-location-record,
         source-position: source-location.source-location-source-position);
  else
    invalid-token(source-location);
  end;
end;

define constant $hash-data-start-delimiters :: <byte-string> = "\"{[(";
define constant $hash-data-end-delimiters   :: <byte-string> = "\"}])";
define constant $escape-code = as(<integer>, '\\');

define method make-hash-literal
    (lexer :: <lexer>, source-location :: <lexer-source-location>)
    => res :: <fragment>;
  let name-string
    = extract-string
        (source-location, 
           start: source-location.start-posn + 1,
           end:   source-location.end-posn - 1);
  format-out("Hash literal: %s\n", name-string);
  block ()
    let contents :: <byte-vector> = lexer.source.contents;
    let length :: <integer> = contents.size;
    let data-start :: <integer> = lexer.posn;
    let delimiter = contents[data-start];
    let delimiter-index 
      = position($hash-data-start-delimiters, as(<character>, delimiter));
    let data :: <byte-string> = "";
    if (delimiter-index)
      // Read until the matching delimiter
      let start-delimiter = delimiter;
      let end-delimiter 
        = as(<integer>, $hash-data-end-delimiters[delimiter-index]);
      let i :: <integer> = data-start + 1;
      let char :: <integer> = 0;
      while (((char := contents[i]) ~== end-delimiter)
               | (char == end-delimiter & contents[i - 1] == $escape-code))
        if (char == $newline-code)
  	  lexer.line := lexer.line + 1;
	  lexer.line-start := i;
        end;
        i := i + 1;
      end;
      data 
        := extract-string(source-location, start: data-start + 1, end: i);
      lexer.posn := i + 1;
    else
      // Read until whitespace
      let i :: <integer> = data-start;
      let char :: <integer> = 0;
      while (~delimiter-code?((char := contents[i])))
        /*
        if (char == $newline-code)
  	  lexer.line := lexer.line + 1;
	  lexer.line-start := i;
        end;
        */
        i := i + 1;
      end;
      data 
        := extract-string(source-location, start: data-start, end: i);
      lexer.posn := i;
    end;
    let module = *Current-Module*;
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
end method;

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
end function;


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
// come across the next flush-left character that doesn't like "end".

// TODO: CORRECTNESS: Multiplatform newline sequence handling.

define constant $space-code = as(<integer>, ' ');
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
end function;

define inline function at-whitespace?
    (contents :: <byte-vector>, posn :: <integer>, length :: <integer>)
 => (whitespace? :: <boolean>)
  posn < length
    & begin
        let c = contents[posn];
        c == $space-code | c == $newline-code | c == $tab-code
      end;
end function;

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
end function;

define method skip-to-next-top-level-form (lexer :: <lexer>) => ()
  let contents = lexer.source.contents;
  let length = contents.size;
  local method walk (i :: <integer>)
    if (i < length)
      let (newline?, start) = at-newline?(contents, i);
      if (newline?)
        lexer.line-start := start;
        lexer.line := lexer.line + 1;
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
  lexer.posn := walk(lexer.posn);
end method;
