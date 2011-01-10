Module: C-lexer-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(state-vector-setter);

// ----------------------------------------------------------------------
// State information --

// <C-lexer>s are wrapper streams.  The state-vector is everything about
// the state except the inner-stream.  Makes it possible to pass the state
// to a new stream when switching include files.  Which really isn't
// necessary since no token ever spans two include files.  The #include
// syntax implicitly guarantees this.

define abstract primary class <C-lexer> (<wrapper-stream>)
  slot source-name :: <string>, init-keyword: source-name:, 
    init-value: "unknown stream";
  slot state-vector :: <C-lexer-state-vector>,
    init-keyword: state-vector:, 
    init-function: method() make(<C-lexer-state-vector>) end;
  slot current-line :: <integer>, init-keyword: current-line:;
  slot next-character-line-number :: <integer>,
    init-keyword: next-character-line-number:;
end class;

define method close (the-stream :: <C-lexer>, #key) => ()
  close(the-stream.inner-stream);
end method;

define method print-object 
    (the-lexer :: <C-lexer>, the-stream :: <stream>) => ();
  let the-class-string = print-to-string(object-class(the-lexer));
  let cleaned-up-emulator-string =
    copy-sequence(the-class-string, start: 7, 
		  end: the-class-string.size - 1);
  format(the-stream,
	 "(instance of (%s, current-source: %s, current-line: %=))",
	 cleaned-up-emulator-string,
	 the-lexer.source-name,
	 the-lexer.current-line);
end method;

// Various dialects of <C-lexer>s

define abstract class <Microsoft-C-lexer> (<C-lexer>) end;

define sealed concrete class 
    <actual-Microsoft-C-lexer> (<Microsoft-C-lexer>) 
end;

define method make 
    (class == <Microsoft-C-lexer>, 
     #rest initialization-arguments,
     #key) => (result :: <actual-Microsoft-C-lexer>);
  apply(make, <actual-Microsoft-C-lexer>, initialization-arguments)
end method;

define concrete class <ansi-C-lexer> (<C-lexer>) end;

define sealed concrete class 
    <actual-ansi-C-lexer> (<ansi-C-lexer>) 
end;

define method make 
    (class == <ansi-C-lexer>, 
     #rest initialization-arguments,
     #key) => (result :: <actual-ansi-C-lexer>);
  apply(make, <actual-ansi-C-lexer>, initialization-arguments)
end method;

define abstract class <C++-lexer> (<C-lexer>) end;

define sealed concrete class 
    <actual-C++-lexer> (<C++-lexer>) 
end;

define method make 
    (class == <C++-lexer>, 
     #rest initialization-arguments,
     #key) => (result :: <actual-C++-lexer>);
  apply(make, <actual-C++-lexer>, initialization-arguments)
end method;

// If the pre-lexer stream is newly opened and hasn't inherited a current
// character from state of a previous pre-lexer inner-stream then read the
// first character from the inner-stream.
define method initialize (new-lexer :: <C-lexer>, #key)
  next-method();
  if ( ~ slot-initialized?(new-lexer.state-vector, next-character))
    new-lexer.state-vector.next-character :=
      read-element(new-lexer.inner-stream, on-end-of-stream: #"eoi");
    new-lexer.next-character-line-number :=
      new-lexer.inner-stream.current-line; 
  end;
  if ( ~ slot-initialized?(new-lexer.state-vector, current-character))
    read-next-lex-char(new-lexer);
  end;
end method;


define class <C-lexer-state-vector> (<object>)
  slot current-character ::  type-union(<character>, one-of(#"eoi")),
    init-keyword: current-character:;
  slot next-character ::  type-union(<character>, one-of(#"eoi")),
    init-keyword: next-character:;
  slot  collected-characters :: <list>, 
    init-keyword: collected-characters:, init-value: #();
  slot current-char-was-read? :: <boolean>,
    init-keyword: current-char-was-read?:, init-value: #f;
end class;

define method current-char-was-read? 
    (s :: <C-lexer>) => (result :: <boolean>);
  s.state-vector.current-char-was-read?
end;

define method current-char-was-read?-setter 
    (value :: <boolean>, s :: <C-lexer>);
  s.state-vector.current-char-was-read? := value;
end;

define macro current-lex-char
  { current-lex-char ( ?stream:expression ) }
    => { ?stream.state-vector.current-character }
end;

define macro next-lex-char
  { next-lex-char ( ?stream:expression ) } 
    => { ?stream.state-vector.next-character }
end;

define macro lex-end-of-file?
  { lex-end-of-file? ( ?stream:expression ) }
    => { ?stream.state-vector.current-character = #"eoi" }
end;

// Part of the stream protocol

define method read-element (s :: <c-lexer>, 
			    #key on-end-of-stream)
 => (result);
  extract-token(s);
end;

// read-to-end applied to the inner-stream isn't quite the same as this
// since from the inner-stream's point of view, the current-character and
// next-character have already been read.  Maybe this is a mistake in the
// way I have constructed the lexer streams.  Possibly current-character
// and next-character should be part of the inner-stream's protocol.  To do
// that I would have to add another layer of wrappers between the pre-lexer
// and the lexer to implement current-character and next-character.
// Without that I can't run a lexer over a <string-stream> which is needed in
// the cpp streams.  I don't want to make current-character and
// next-character part of the pre-lexer streams.  This will have to do for
// now.  This is only used in error reporting so far so it doesn't need to
// be efficient.

define method read-to-end-of-inner-stream (s :: <c-lexer>, #key )
 => (result :: <string>);
  concatenate-as(<string>,
		 clex-as-unit-string(if (current-char-was-read?(s))
				  "" else current-lex-char(s)
				end),
		 clex-as-unit-string(next-lex-char(s)),
		 read-to-end(s.inner-stream))
end;

// getting the string turns off character collection!
define method get-lex-string (s :: <C-lexer>)
  let result = as(<string>, reverse!(s.state-vector.collected-characters));
  s.state-vector.collected-characters := #();
  result;
end method get-lex-string;

define method collect-lex-chars (s :: <C-lexer>)
  s.state-vector.collected-characters
    := add!(s.state-vector.collected-characters, current-lex-char(s));
end method collect-lex-chars;

define method read-next-lex-char (s :: <C-lexer>)
  s.state-vector.current-character := s.state-vector.next-character;
  s.current-line := s.next-character-line-number;
  s.state-vector.next-character
    := read-element(s.inner-stream, on-end-of-stream: #"eoi");
  s.next-character-line-number := s.inner-stream.current-line;
  if (~empty?(s.state-vector.collected-characters))
    collect-lex-chars(s); 
  end if;
end method read-next-lex-char;

define method lex-error (s :: <C-lexer>, string, #rest format-arguments)
  apply(error, 
	concatenate("line %= : current-char %= : token so far %= : ", string), 
	current-line(s),
	current-lex-char(s),
	as(<string>, reverse(s.state-vector.collected-characters)), next-lex-char(s),
	format-arguments);
end method lex-error;

define method lex-warning (s :: <C-lexer>, string, #rest format-arguments)
  apply(signal, 
	concatenate("line %= : current-char %= : token so far %= : ", string), 
	current-line(s),
	current-lex-char(s),
	as(<string>, reverse(s.state-vector.collected-characters)), next-lex-char(s),
	format-arguments);
end method lex-warning;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//  white space
//  ({horizontal_white}|{v_tab}|{c_return}|{form_feed}|"\n")+   ;
define method read-white-space (s :: <C-lexer>)
  let token-class 
    = if (current-lex-char(s) = '\n') <new-line> else <space> end if;
  while (clex-white-space?(next-lex-char(s)))
    read-next-lex-char(s);
    if(current-lex-char(s) = '\n') token-class := <new-line> end if;
  end while;
  make(token-class, source-line: s.current-line);
end method read-white-space;

    
define method read-slash-slash-comment (s :: <C-lexer>)
  // current character is the opening '/'
  read-next-lex-char(s);
  // current character is the second '/'
  read-next-lex-char(s);
  until ((current-lex-char(s) = #"eoi") | (current-lex-char(s) = '\n'))
    read-next-lex-char(s);
  end until;
  make(<new-line>, source-line: s.current-line)
end method;

define method read-slash-star-comment (s :: <C-lexer>)
  // current character is the opening '/'
  read-next-lex-char(s);
  // current character is the following '*'
  read-next-lex-char(s);
  until (((current-lex-char(s) = '*') & (next-lex-char(s) = '/')) | (current-lex-char(s) = #"eoi"))
    if (next-lex-char(s) = #"eoi")
      lex-warning(s, "end of input in comment");
    end if;
    read-next-lex-char(s);
  end until;
  read-next-lex-char(s);
  make(<space>, source-line: s.current-line)
end method;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//  Strings 
// Reading doesn't strip escapes.  Dylan-value converts the input-string
// into a dylan string and strips the escapes.
define method lex-read-character-literal (s :: <C-lexer>)
  // current character is the opening single quote
  let lex-state = $S0;
  read-next-lex-char(s);
  until (lex-state == $SFINISHED)
    select (lex-state by \==)
      $S0 =>
	select (current-lex-char(s) by \==)
	  '\\' => lex-state := $S1; read-next-lex-char(s);
	  '\'' => lex-state := $SFINISHED;
	  '\n' => lex-error(s, "new line in character literal");
	  #"eoi" => lex-error(s, "end of input in character literal"); 
	  otherwise => read-next-lex-char(s); // still $S0
	end select;
      $S1 =>
	select (current-lex-char(s) by \==)
	  '\n' => lex-error(s, "new line in character literal");
	  #"eoi" => lex-error(s, "end of input in character literal"); 
	  otherwise => lex-state := $S0; read-next-lex-char(s); 
	end select;
      otherwise => lex-error(s, "unknown lexer state in character literal"); 
    end select;
  end until;
  make(<character-literal>, source-line: s.current-line,
       lexer-string: get-lex-string(s));
end method lex-read-character-literal;

// Remember escaped new-lines are stripped by the pre-lexer
define method lex-read-string-literal (s :: <C-lexer>)
  // current character is the opening double quote
  let lex-state = $S0;
  read-next-lex-char(s);
  until (lex-state == $SFINISHED)
    select (lex-state by \==)
      $S0 =>
	select (current-lex-char(s) by \==)
	  '\\' => lex-state := $S1; read-next-lex-char(s);
	  '"' => lex-state := $SFINISHED;
	  '\n' => lex-error(s, "new line in string literal");
	  #"eoi" => lex-error(s, "end of input in string literal"); 
	  otherwise => read-next-lex-char(s); // still $S0
	end select;
      $S1 =>
	select (current-lex-char(s) by \==)
	  '\n' => lex-error(s, "new line in string literal");
	  #"eoi" => lex-error(s, "end of input in string literal"); 
	  otherwise => lex-state := $S0; read-next-lex-char(s); 
	end select;
      otherwise => lex-error(s, "unknown lexer state in string literal"); 
    end select;
  end until; 
  make(<string-literal>, source-line: s.current-line,
       lexer-string: get-lex-string(s));
end method lex-read-string-literal;

// ----------------------------------------------------------------------
//  C++ keywords

/*(#(#("class", #"class"), #("template", #"template"),
                       #("delete", #"delete"), #("friend", #"friend"),
                       #("inline", #"inline"), #("new", #"new"),
                       #("operator", #"operator"), #("overload", #"overload"),
                       #("protected", #"protected"), #("private", #"private"),
                       #("public", #"public"), #("this", #"this"),
                       #("virtual", #"virtual")));
*/

// ----------------------------------------------------------------------
//  indentifiers + keywords

define method lex-read-identifier (s :: <C-lexer>)
  collect-lex-chars(s);
  for (while: clex-alphanumeric?(next-lex-char(s)))
    read-next-lex-char(s);
  end for;
  make(<identifier>, source-line: s.current-line,
       lexer-string: get-lex-string(s))
end method lex-read-identifier;

// ----------------------------------------------------------------------
//  Symbols

define method lex-read-symbol (s :: <C-lexer>)
  select(current-lex-char(s))
    '(' => make(<open-parenthesis>, source-line: s.current-line);
    ')' => make(<close-parenthesis>, source-line: s.current-line);
    ',' => make(<comma>, source-line: s.current-line);
    '{' => make(<open-brace>, source-line: s.current-line);
    '}' => make(<close-brace>, source-line: s.current-line);
    '[' => make(<open-bracket>, source-line: s.current-line);
    ']' => make(<close-bracket>, source-line: s.current-line);
    '.' => 
      if (next-lex-char(s) == '*') read-next-lex-char(s); 
	make(<dot-star>, source-line: s.current-line)
      elseif (next-lex-char(s) == '.')
	read-next-lex-char(s);
	if (next-lex-char(s) == '.') 
	  read-next-lex-char(s);
          make(<ellipsis>, source-line: s.current-line);
	else 
	  // two dots in a row.  Illegal?  Anyway we've read one character
	  // so prevent extract-token from reading yet another.
	  current-char-was-read?(s) := #f;
	  make(<dot>, source-line: s.current-line);
	end if;
      else make(<dot>, source-line: s.current-line)
      end if;
    '&' => 
      select (next-lex-char(s))
	'&' => read-next-lex-char(s); 
	  make(<and-and>, source-line: s.current-line);
	'=' => read-next-lex-char(s); 
	  make(<and-assign>, source-line: s.current-line);
	otherwise => make(<and>, source-line: s.current-line);
      end select;
    '*' =>
      select (next-lex-char(s))
	'=' => read-next-lex-char(s); 
	  make(<multiply-assign>, source-line: s.current-line);
	otherwise => make(<star>, source-line: s.current-line);
      end select;
    '+' =>
      select (next-lex-char(s))
	'+' => read-next-lex-char(s); 
	  make(<plus-plus>, source-line: s.current-line);
	'=' => read-next-lex-char(s); 
	  make(<plus-assign>, source-line: s.current-line);
	otherwise => make(<plus>, source-line: s.current-line);
      end select;
    '-' =>
      select (next-lex-char(s))
	'-' => read-next-lex-char(s); 
	  make(<minus-minus>, source-line: s.current-line);
	'>' => 
	  read-next-lex-char(s); 
	  select (next-lex-char(s))
	    '*' => 
	      if (instance?(s, <C++-lexer>))
		read-next-lex-char(s); 
		make(<arrow-star>, source-line: s.current-line);
	      else
		make(<arrow>, source-line: s.current-line);
	      end if;
	    otherwise => make(<arrow>, source-line: s.current-line);
	  end select;
	'=' => read-next-lex-char(s);
	  make(<minus-assign>, source-line: s.current-line);
	otherwise =>  make(<minus>, source-line: s.current-line);
      end select;
    '~' => make(<tilde>, source-line: s.current-line);
    '!' => 
      select (next-lex-char(s))
	'=' => read-next-lex-char(s);
	  make(<not-equal>, source-line: s.current-line);
	otherwise => make(<not>, source-line: s.current-line);
      end select;
    '/' =>
      select (next-lex-char(s))
	'=' => read-next-lex-char(s); 
	  make(<divide-assign>, source-line: s.current-line);
	otherwise => make(<divide>, source-line: s.current-line);
      end select;
    '%' => 
      select (next-lex-char(s))
	'=' => read-next-lex-char(s); 
	  make(<remainder-assign>, source-line: s.current-line);
	otherwise => make(<remainder>, source-line: s.current-line);
      end select;
    '<' => 
      select (next-lex-char(s))
	'=' => read-next-lex-char(s);
	  make(<less-than-or-equal>, source-line: s.current-line);
	'<' => 
	  read-next-lex-char(s);
	  select (next-lex-char(s))
	    '=' => read-next-lex-char(s); 
	      make(<left-shift-assign>, source-line: s.current-line);
	    otherwise => make(<left-shift>, source-line: s.current-line);
	  end select;
	otherwise => make(<less-than>, source-line: s.current-line);
      end select;
    '>' => 
      select (next-lex-char(s))
	'=' => read-next-lex-char(s) ; 
	  make(<greater-than-or-equal>, source-line: s.current-line);
	'>' => 
	  read-next-lex-char(s);
	  select(next-lex-char(s))
	    '=' => read-next-lex-char(s); 
	      make(<right-shift-assign>, source-line: s.current-line);
	    otherwise => make(<right-shift>, source-line: s.current-line);
	  end select;
	otherwise => make(<greater-than>, source-line: s.current-line);
      end select;
    '^' =>
      select (next-lex-char(s))
	'=' => read-next-lex-char(s); 
	  make(<carat-assign>, source-line: s.current-line);
	otherwise => make(<carat>, source-line: s.current-line);
      end select;
    '|' => 
      select (next-lex-char(s))
	'=' => read-next-lex-char(s);
	  make(<or-assign>, source-line: s.current-line);
	'|' => read-next-lex-char(s);
	  make(<or-or>, source-line: s.current-line);
	otherwise => make(<or>, source-line: s.current-line);
      end select;
    '?' => make(<question>, source-line: s.current-line);
    ':' => 
      select (next-lex-char(s))
	':' => read-next-lex-char(s); 
	  make(<colon-colon>, source-line: s.current-line);
	otherwise => make(<colon>, source-line: s.current-line);
      end select;
    ';' => make(<semi-colon>, source-line: s.current-line);
    '=' => 
      select (next-lex-char(s))
	'=' => read-next-lex-char(s); 
	  make(<equal-equal>, source-line: s.current-line);
	otherwise => make(<equal>, source-line: s.current-line);
      end select;
    otherwise => #f;
  end select;
end method lex-read-symbol;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//  Numbers
// 
// exponent_part ([eE][-+]?[0-9]+)
// fractional_constant ([0-9]*"."[0-9]+)|([0-9]+".")
// floating_constant ((({fractional_constant}{exponent_part}?)|([0-9]+{exponent_part}))[FfLl]?)
// 
// integer_suffix_opt (([uU]?[lL]?)|([lL][uU]))
// decimal_constant ([1-9][0-9]*{integer_suffix_opt})
// octal_constant ("0"[0-7]*{integer_suffix_opt})
// hex_constant ("0"[xX][0-9a-fA-F]+{integer_suffix_opt})

// exponent_part ([eE][-+]?[0-9]+)
define method lex-read-exponent-part (s :: <C-lexer>)
  read-next-lex-char(s);
  if (next-lex-char(s) = '+' | next-lex-char(s) = '-')
    read-next-lex-char(s);
  end if;
  if (clex-digit?(next-lex-char(s)))
    read-next-lex-char(s);
    while (clex-digit?(next-lex-char(s))) read-next-lex-char(s); end;
  else
    lex-error(s, "floating point exponent : digit expected!");
  end if;
end method lex-read-exponent-part;

define method lex-exponent-part-p (char)
  char = 'e' | char = 'E';
end method lex-exponent-part-p;

define method lex-read-float-size-info (s :: <C-lexer>)
  if (next-lex-char(s) = 'F' | next-lex-char(s) = 'f' | next-lex-char(s) = 'L'
       | next-lex-char(s) = 'l')
    read-next-lex-char(s);
  end if;
end method lex-read-float-size-info;

define method lex-read-float-fractional-constant (s :: <C-lexer>)
  // next-lex-char is = '.'
  let has-exponent? = #f;
  read-next-lex-char(s); 
  // current-lex-char is = '.'
  while (clex-digit?(next-lex-char(s))) read-next-lex-char(s); end while;
  if (lex-exponent-part-p(next-lex-char(s)))
     has-exponent? := #t;
    //  optional
    lex-read-exponent-part(s);
  end if;
  lex-read-float-size-info(s);
  make(<float-literal>, source-line: s.current-line, 
       has-exponent?: has-exponent?, lexer-string: get-lex-string(s));
end method lex-read-float-fractional-constant;

define method lex-read-float-exponent (s :: <C-lexer>)
  lex-read-exponent-part(s);
  lex-read-float-size-info(s);
  make(<float-literal>, source-line: s.current-line, 
       has-exponent?: #t, lexer-string: get-lex-string(s));
end method lex-read-float-exponent;

define method lex-read-float (s :: <C-lexer>)
  while (clex-digit?(next-lex-char(s))) read-next-lex-char(s); end while;
  if (next-lex-char(s) = '.')
    lex-read-float-fractional-constant(s);
  elseif (lex-exponent-part-p(next-lex-char(s)))
    lex-read-float-exponent(s);
  else
    lex-error(s, "reading floating point number: one of [.eE] expected");
  end if;
end method lex-read-float;

// This hack depends on being called in an environment character collection
// has been turned off my calling get-lex-string before hand.  This is
// appallingly awful.  Should really just use a form of read-next-lex-char
// which doesn't collect -- call it skip-next-lex-char maybe
define method lex-read-integer-suffix-opt (s :: <C-lexer>)
  if (next-lex-char(s) = 'u' | next-lex-char(s) = 'U')
    read-next-lex-char(s);
    if (next-lex-char(s) = 'l' | next-lex-char(s) = 'L')
      read-next-lex-char(s);
    end if;
  elseif (next-lex-char(s) = 'l' | next-lex-char(s) = 'L')
    read-next-lex-char(s);
    if (next-lex-char(s) = 'u' | next-lex-char(s) = 'U')
      read-next-lex-char(s);
    end if;
  end if;
end method lex-read-integer-suffix-opt;

define method lex-read-octal-or-float 
    (s :: <C-lexer>) => (result :: <token>);
  collect-lex-chars(s);
  while (clex-octal-digit?(next-lex-char(s)))
    read-next-lex-char(s);
  end while;
  // Test whether it is an octal integer or really a decimal float
  // beginning with a 0.
  if (next-lex-char(s) = '8' | next-lex-char(s) = '9')
    lex-read-float(s)
  elseif (next-lex-char(s) = '.')
    lex-read-float-fractional-constant(s)
  elseif (lex-exponent-part-p(next-lex-char(s)))
    lex-read-float-exponent(s)
  else
    // it really is an octal constant after all
    let result = get-lex-string(s);
    // collecting is off so skip any suffix.  Maybe not a good idea to lose
    // this information!
    lex-read-integer-suffix-opt(s);
    make(<octal-integer-literal>, source-line: s.current-line, 
	       lexer-string: result);
  end if;
end method lex-read-octal-or-float;

define method lex-read-decimal-or-float 
    (s :: <C-lexer>) => (result :: <token>)
  collect-lex-chars(s);
  while (clex-digit?(next-lex-char(s)))
    read-next-lex-char(s);
  end while;
  if (next-lex-char(s) = '.')
    lex-read-float-fractional-constant(s)
  elseif (lex-exponent-part-p(next-lex-char(s)))
    lex-read-float-exponent(s)
  else
    // it really is a decimal constant
    let result = get-lex-string(s);
    // collecting is off so skip any suffix.  Maybe not a good idea to lose
    // this information!
    lex-read-integer-suffix-opt(s);
    make(<decimal-integer-literal>, source-line: s.current-line, 
	 lexer-string: result);
  end if;
end method lex-read-decimal-or-float;

define method lex-read-hex (s :: <C-lexer>)
  // current-lex-char = '0', next-lex-char = 'x' | 'X'
  read-next-lex-char(s);
  // current-lex-char = 'x' | 'X'
  if (clex-hex-digit?(next-lex-char(s)))
    read-next-lex-char(s);
    // current-lex-char is a digit, turn on collection so that the first
    // collected character is the first digit.  Now push current-lex-char
    // onto collected-lex-chars and turn on collection.
    collect-lex-chars(s);
    while (clex-hex-digit?(next-lex-char(s)))
      read-next-lex-char(s);
    end while;
    // at the end of this loop current-lex-char has the last hex digit
    // which is the same as the first character in collected-lex-chars
    let result = get-lex-string(s);
    // collecting is off so skip any suffix.  Maybe not a good idea to lose
    // this information!
    lex-read-integer-suffix-opt(s);
    make(<hexadecimal-integer-literal>, source-line: s.current-line, 
	 lexer-string: result)
  else
    lex-error(s, "hex digit expected");
  end if;
end method lex-read-hex;

define method lex-read-number (s :: <C-lexer>) => (result :: <token>)
  if (current-lex-char(s) = '0')
    if (next-lex-char(s) = 'x' | next-lex-char(s) = 'X')
      lex-read-hex(s);
    else
      lex-read-octal-or-float(s);
    end if;
  else
    lex-read-decimal-or-float(s);
  end if;
end method lex-read-number;

// ----------------------------------------------------------------------
//  Top level -- We could make this be a single top level switch as long as
//  read symbol is the last test and the otherwise default error is cut
//  contained in lex-read-symbol... of course integrating the cpp stuff may
//  louse that up.  The clex-alpha? test doesn't work that way either
define method extract-token (s :: <C-lexer>)
  let token = #f;
  s.state-vector.collected-characters :=  #();
  if ( current-char-was-read?(s)) read-next-lex-char(s); end if;
  current-char-was-read?(s):= #t;
  if (clex-white-space?(current-lex-char(s)))
    read-white-space(s);	
  elseif ((current-lex-char(s) = '/') & (next-lex-char(s) = '/'))
    read-slash-slash-comment(s)
  elseif ((current-lex-char(s) = '/') & (next-lex-char(s) = '*'))
    read-slash-star-comment(s)
  elseif ((current-lex-char(s) = 'L') & (next-lex-char(s) = '\''))
    //  Wide chars 
    collect-lex-chars(s);
    read-next-lex-char(s);
    lex-read-character-literal(s);
  elseif ((current-lex-char(s) = 'L') & (next-lex-char(s) = '"'))
    collect-lex-chars(s);
    read-next-lex-char(s);
    lex-read-string-literal(s);
  elseif (clex-alpha?(current-lex-char(s))
           | '_' = current-lex-char(s))
    lex-read-identifier(s)
  elseif (clex-digit?(current-lex-char(s))
           | (current-lex-char(s) = '.' & clex-digit?(next-lex-char(s))))
    lex-read-number(s)
  elseif (current-lex-char(s) = '\'')
    collect-lex-chars(s);
    lex-read-character-literal(s)
  elseif (current-lex-char(s) = '"')
    collect-lex-chars(s);
    lex-read-string-literal(s)
  elseif (current-lex-char(s) = '#')
    if (next-lex-char(s) = '#')
      read-next-lex-char(s);
      make(<pound-pound>, source-line: s.current-line)
    else
      make(<pound>, source-line: s.current-line)
    end if;
  elseif (token := lex-read-symbol(s))
    token
  elseif (lex-end-of-file?(s))
    make(<eoi>, source-line: s.current-line);
  else
    lex-error(s, "????????")
  end if;
end method extract-token;

// Include file names have different lexical properties from other tokens.
// This function returns false if the filename isn't either a
// <ordinary-filename>  ("...") or a <standard-filename> (<...>).
define method read-include-filename 
    (s :: <C-lexer>, #key relexing-expanded-tokens? :: <boolean> = #f )
 => (result :: false-or(<token>));
  s.state-vector.collected-characters :=  #();
  if ( current-char-was-read?(s)) read-next-lex-char(s); end if;
  current-char-was-read?(s):= #f;
  while (clex-white-space?(current-lex-char(s)) & current-lex-char(s) ~= '\n')
    read-next-lex-char(s);
  end while;
  select(current-lex-char(s))
    '"' =>
      collect-lex-chars(s);
      read-next-lex-char(s); 
      for (until: current-lex-char(s) = '"' | current-lex-char(s) = '\n' 
	     | current-lex-char(s) =  #"eoi")
	read-next-lex-char(s); 
      finally 
	current-char-was-read?(s):= #t;
	if (current-lex-char(s) = '"')
	  make(<ordinary-filename>, source-line: s.current-line,
	       lexer-string: get-lex-string(s))
	else 
	  lex-error(s, "badly formed include filename");
	end if
      end for;
    '<' =>
      collect-lex-chars(s);
      read-next-lex-char(s); 
      for (until: current-lex-char(s) = '>' | current-lex-char(s) = '\n' 
	     | current-lex-char(s) =  #"eoi")
	read-next-lex-char(s); 
      finally 
	current-char-was-read?(s) := #t;
	if (current-lex-char(s) = '>')
	  make(<standard-filename>, source-line: s.current-line,
	       lexer-string: get-lex-string(s))
	else 
	  lex-error(s, "badly formed include filename");
	end if;
      end for;   
    '\n', #"eoi" =>
      current-char-was-read?(s) := #t;
      make(<empty-token>, source-line: s.current-line);
    otherwise =>
      if (relexing-expanded-tokens?)
	lex-error(s, "Tokens following #include don't macro expand to"
		    "either \"file-name\" or <file-name>");
      else
	current-char-was-read?(s) := #f;
	#f
      end if
  end select;
end method;

/* This is clearly bogus but I don't think it is needed unless we try to
   use the C++ parser.

define method c++-lexer-skip-to (token, nested)
  let a = #f;
  let b = #f;
  let count :: <integer> = 1;
  for (until begin
               apply(method (#key g9599, g9600, #rest m-v-b-&rest9601)
                       a := g9599;
                       b := g9600;
                       g9599;
                     end method,
                     concatenate!(begin
                                    let (#rest _) = extract-token(s);
                                    _;
                                  end));
               empty?(a)
                | begin
                    if (a == token)
                      dec!(count);
                    elseif (a == nested)
                      inc!(count);
                    end if;
                    count = 0;
                  end;
             end);
  end for;
  values(a, b);
end method c++-lexer-skip-to;
*/

/* useful for hand testing */
define method make-test-lexer-stream (string, #key lexer-class = <ansi-C-lexer>)
  make(lexer-class,
       source-name: "lexer test string",
       inner-stream: make(<pre-lexer>, 
			  inner-stream: make(<string-stream>,
					     direction: #"input",
					     contents: string)))
end method;

define method test-lexer (string, #key lexer-class = <ansi-C-lexer>)
  let stream = make-test-lexer-stream(string, lexer-class: lexer-class);
  let token-list = #();
  let token = read-element(stream);
  while (object-class(token) ~= <eoi>)
    token-list := 
      add!(token-list, token);
    token := read-element(stream);
  end while;
  token-list := 
    add!(token-list, token);
  reverse!(token-list);
end method test-lexer;

define method lex-from-string 
    (the-string :: <string>, lexer-class :: <class>, // subtype(<C-lexer>)
     #key source-name: input-source-name :: false-or(<string>) = #f)
 => (result-token :: <token-list>);
  let the-source-name =
    if (input-source-name) input-source-name
    else concatenate("unnamed lexer over string input: ", the-string)
    end if;
  let the-stream =
    make(lexer-class,
	 source-name: the-source-name,
	 inner-stream:
	   make(<pre-lexer>, 
		source-name: the-source-name,
		inner-stream: 
		  make(<string-stream>,
		       direction: #"input",
		       contents: the-string)));
  let the-token-list = make(<token-list>);
  let the-token = read-element(the-stream);
  until (instance?(the-token, <eoi>))
    push-last(the-token-list, the-token);
    the-token := read-element(the-stream);
  end until;
  the-token-list
end method;




