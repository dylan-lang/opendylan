Module: C-lexer-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(source-name-setter, lexer-warning);

// This file contains stream, state and state machine lexer definitions for
// pre-lexing a character input stream.  Pre-lexing is required for ISO C
// parsing.  It removes escaped line-breaks and handles trigraphs and trigraph
// escapes.  It also keeps the line counter so that line numbers reported
// in error messages match the source before pre-lexing and macro
// expansion.  To see that this is really a separate pass consider that:
// 
//    "??/" => "\"
//    "\<lf>"  => "" (escaped new-line)
//    "??/<lf>" => "" (escaped new-line) 
//    "\??/" => "??/"
//    "?\?/" => "??/"
//    "??/??/" => "\\"
// 
// In particular it is not the case that "??/??/" is interpreted as "\??/"
// and then reduced to "??/".  Unlike what is done for escaping new lines
// using "??/" as equivalent to "\".  This takes much more look ahead than
// the lexer is prepared for.

// FSM states

define constant <pre-lexer-state> = <integer>;

define constant  $S0 =  0;
define constant  $S1 =  1;
define constant  $S2 =  2;
define constant  $S3 =  3;
define constant  $S4 =  4;
define constant  $S5 =  5;
define constant  $S6 =  6;
define constant  $S7 =  7;
define constant  $SFINISHED = 8;

define macro current-char
  { current-char ( ?stream:name ) } 
    => { ?stream.current-character }
end macro;

define macro current-state
  { current-state ( ?stream:name ) } => { ?stream.state }
end macro;

define macro next-line
  { next-line ( ?stream:name ) } 
    => { ?stream.line-number 
	  := ?stream.line-number + 1 }
end macro;

define macro next-state
  { next-state ( ?stream:name, ?state-number:name ) } 
    => { ?stream.state := ?state-number }
end macro;

define macro next-char
  { next-char ( ?stream:name ) } 
    => { ?stream.current-character 
	   := read-element(?stream.inner-stream, on-end-of-stream: #"eoi") 
	   // := read-element-from-file-stream(?stream)
	  }
end macro;

/* OBSOLETE
// metering trampoline
define method read-element-from-file-stream(stream)
  read-element(stream.inner-stream, on-end-of-stream: #"eoi")
end method;
*/

/* UNUSED
define macro new-current-char
  { new-current-char ( ?stream:name, ?character:expression ) } 
    => { ?stream.current-character 
	   := ?character
	  }
end macro;
*/

// <pre-lexer>s are wrapper streams.
define class <pre-lexer> (<wrapper-stream>)
  slot source-name :: <string>, init-keyword: source-name:, 
    init-value: "unknown stream";
  slot line-number :: <integer>,
    init-keyword: line-number:, init-value: 1;
  slot current-character ::  type-union(<character>, one-of(#"eoi"), 
					one-of(#f)),
    init-keyword: current-character:, init-value: #f;
  slot state :: <pre-lexer-state>, init-keyword: state:, init-value: $S0;
end class;

define method close (the-stream :: <pre-lexer>, #key) => ()
  close(the-stream.inner-stream);
end method;

define method print-object 
    (the-described-stream :: <pre-lexer>, the-stream :: <stream>) => ();
end method;

define method current-line (stream :: <pre-lexer>) => (result :: <integer>)
  stream.line-number
end method;

// This needs a little more thought if it ever becomes important.  The
// current-character is frequently already read when the streams are
// switched so one further character on the old stream is read.  Also it
// isn't clear what should happen when the stream is in a push-down state.
define method inner-stream-setter 
    (the-inner-stream :: <stream>, the-outer-stream :: <pre-lexer>) 
 => (result :: <stream>);	// SML 10/13/97: was: false-or(<stream>)
  let result = next-method();
  if ( ~ current-char(the-outer-stream) 
	| (current-char(the-outer-stream) = #"eoi"))
    next-char(the-outer-stream);
  end if;
  result
end method;

// If the pre-lexer stream is newly opened and hasn't inherited a current
// character from state of a previous pre-lexer inner-stream then read the
// first character from the inner-stream.
define method initialize (new-lexer :: <pre-lexer>, #key)
  next-method();
  if ( ~ current-char(new-lexer) )
    next-char(new-lexer);
  end if;
end method;

define method read-element (s :: <pre-lexer>, #key on-end-of-stream = #"eoi")
 =>(result :: <object>);
  let result = #f;
  while ( ~ result )
    select (current-state(s))
      $S0 =>
	select (current-char(s))
	  '\\' => next-state(s, $S1); next-char(s);
	  '?' => next-state(s, $S4); next-char(s);
	  '\n' => result := '\n'; next-line(s); next-char(s); 
	  '\r' => next-line(s); next-state(s, $S3); next-char(s);
	  #"eoi" => result := on-end-of-stream;
	  otherwise => result := current-char(s); next-char(s);
	end select;
      $S1 => // "\\" seen -- look for escaped new lines or ?s
	select (current-char(s))
	  '\n' => next-line(s); next-state(s, $S0); next-char(s); 
	  '\r' => next-line(s); next-state(s, $S2); next-char(s);
	  '?'  => result := '?'; next-state(s, $S0); next-char(s);
	  otherwise => result := '\\'; next-state(s,$S0);
	end select;
      $S2 => // "\\\r"  seen -- escaped cr/lf?
	select (current-char(s))
	  '\n' => next-state(s, $S0); next-char(s);
	  otherwise => next-state(s, $S0); 
	end select;
      $S3 => // "\r"  seen from $S0  --  cr/lf?
	select (current-char(s))
	  '\n' => next-char(s); result := '\n'; next-state(s, $S0);
	  otherwise => result := '\n'; next-state(s, $S0); 
	end select;
      $S4 => // "?"  seen -- trigraph beginning from $S0?
	select (current-char(s))
	  '?' => next-state(s, $S5); next-char(s);
	  otherwise => result := '?'; next-state(s,$S0); 
	end select;
      $S5 => // "??"  seen -- trigraph beginning from $S0?
	select (current-char(s))
	  '(' => result := '['; next-state(s, $S0); next-char(s);
	  '<' => result := '{'; next-state(s, $S0); next-char(s);
	  '/' => next-state(s, $S7); next-char(s);
	  '\'' => result := '^'; next-state(s, $S0); next-char(s);
	  '=' => result := '#'; next-state(s, $S0); next-char(s);
	  ')' => result := ']'; next-state(s, $S0); next-char(s);
	  '>' => result := '}'; next-state(s, $S0); next-char(s);
	  '!' => result := '|'; next-state(s, $S0); next-char(s);
	  '-' => result := '~'; next-state(s, $S0); next-char(s);
	  otherwise => result := '?'; next-state(s,$S6); 
	end select;
      $S6 =>
	result := '?'; next-state(s, $S0); 
      $S7 => // "??/"  seen -- trigraph beginning from $S0? Don't accept
	      // this as escaping a ? but do let it escape new-lines
	select (current-char(s))
	  '\n' => next-line(s); next-state(s, $S0); next-char(s); 
	  '\r' => next-line(s); next-state(s, $S2); next-char(s);
	  otherwise => result := '\\'; next-state(s, $S0);
	end select;
      otherwise => 
	error("internal error: unrecognized pre-lexer state %= \n",
	      current-state(s));
    end select;
  end while;
  result
end method read-element;

define method stream-at-end? (s :: <pre-lexer>) => (at-end? :: <boolean>)
  (current-state(s) = $S0) & (current-char(s) = #"eoi")
end method stream-at-end?;

/* OBSOLETE
define method read-to-end(s :: <pre-lexer>)
 =>(result :: <stretchy-vector>);
  let result = make(<stretchy-vector>);
  let the-character = read-element(s);
  while(the-character ~= #"eoi")
    add!(result, the-character);
    the-character := read-element(s);
  end while;
  result
end method;
*/
	
define method lexer-warning (pre-lexer :: <pre-lexer>,
			     format-string :: <string>,
			     #rest format-arguments)
  format-out("\n\npre-lexer warning on line: %= of file: %s",
	     pre-lexer.current-line,
	     pre-lexer.source-name);
  apply(signal, format-string, format-arguments);
end method;

define method test-pre-lexer (string)
  let stream = make(<pre-lexer>,
		    inner-stream:
		      make(<string-stream>, direction: #"input",
			   contents: string));
  let result-list = #();
  let current-character = read-element(stream);
  while (current-character ~=  #"eoi")
    result-list := add!(result-list,  current-character);
    current-character := read-element(stream);
  end while;
  result-list := add!(result-list,  current-character);
  close(stream);
  values(reverse!(result-list),
	 current-line(stream));
end method test-pre-lexer;
 
