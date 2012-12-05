Module: C-lexer-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// hack!!
define constant <machine-word> = <integer>;

ignorable(has-exponent?-setter);

// Tokens support the following protocols:
//
// lexer-string <token> => <string>
//
// This is the string returned from the lexer.  The string is the
// same as the input after the pre-lexer has run. string and character
// literals include enclosing quote marks and escapes are not interpreted.
// This raw form of input is needed to support token merging via the ##
// operator in cpp expansions.
//
// quoted-string <token> => <string>
//
// String as returned from the # string-ization operator in cpp.  Initial
// double quotes, embedded double quotes and embedded \ characters in both
// character and string literals are escaped.  Otherwise the same as
// lexer-string.
//
//   "a \n string" => "\" \\n string\""
//
// dylan-value <literal-token> => type-union (<number>, <string>,
// <character>)
//
// Literals are converted to dylan equivalents, number literals to <float>
// or <machine-word>, character literals to <character> and string literals
// to <string>.  Lexer-strings for character and string literals are
// converted to dylan-values by removing surrounding quotations and
// interpreting escape sequences.
//
// constant-value type-union(<identifier>, <literal-token>) => <machine-word>
//
// returns compile time constant values for character and integer literals
// and 0 for not #define'd identifiers.  this is used to evaluate
// compile time constant expansions in #if expressions.
//
// source-line (token :: <token>) => (result :: false-or(<integer>));
//
// parser-tag (token :: <token>) => (result :: <symbol>);
//
// token-print-string (token :: <token>) => (result :: <string>);
//
// Used to implement print-object in the emulator.
//

define sealed generic
    source-line (token :: <token>) => (result :: false-or(<integer>));

// someday maybe make the source-line be a push down stack so that source
// lines for tokens which are created during macro expansion can keep track
// of locations in the macro definitions as well.
define open abstract primary class <token> (<object>)
  slot source-line :: false-or(<integer>),
    init-keyword:  source-line:, init-value: #f;
end;

// lexer-string and parser-tag are sometimes slots and sometimes attributes
// of classes

define sealed generic lexer-string
    (token :: <token>) => (result :: <string>);

define open generic parser-tag
    (token :: <token>) => (result :: <symbol>);

define sealed generic dylan-value
    (token :: <token>) => (result :: type-union(<character>, <real>,
                                                <string>, <machine-word>));

define sealed generic quoted-string
    (token :: <token>) => (result :: <string>);

// catch all for tokens without more specific methods
define method quoted-string (token :: <token>) => (result :: <string>);
  token.lexer-string
end method;

// white space tokens are needed for macro expansion, especially in the
// expansion of # and in the recognition of function macros.  white space
// tokens are discarded before parsing.  indeed it is possible that there
// is only one actual white space token...

define sealed generic copy-token
    (source-token :: <token>,
     #key /* source-line: the-source-line :: false-or(<integer>) */)
 => (result-token :: <token>);


// this method is all that is needed for tokens with only constant slots
// (other than the source-line).
define method copy-token
    (source-token :: <token>,
     #key source-line: the-source-line :: false-or(<integer>) = #f)
 => (result-token :: <token>)
  make(type-for-copy(source-token),
       source-line:
         if(the-source-line) the-source-line
         else source-token.source-line end if)
end method;

define method print-object
    (the-token :: <token>, the-stream :: <stream>)
 => ();
  format(the-stream, "(instance of (%s, source-line: %=))",
         token-print-string(the-token),
         the-token.source-line);
end method;

define method token-print-string
    (the-token :: <token>) => (result :: <string>)
  let the-class-string = print-to-string(object-class(the-token));
  copy-sequence(the-class-string, start: 7, end: the-class-string.size - 1)
end method;

define abstract class <white-space> (<token>) end class;

// mixin class for new-line and eoi tokens both of which terminate macro
// definitions.
define abstract class <macro-terminator> (<object>) end class;

define class <space> (<white-space>)
  constant slot lexer-string :: <string> = " ";
  constant slot parser-tag :: <symbol> = #"space";
end class;

// empty tokens are used during macro expansion to represent null arguments
// to function macros.
define class <empty-token> (<white-space>)
  constant slot lexer-string :: <string> = "";
  constant slot parser-tag :: <symbol> = #"empty-token";
end class;

// the lexer-string for new-line tokens is space.  this is correct for the
// behavior of the # operator during macro expansion.  any sequence of
// white-space character is reduced to a single space.
define class <new-line> (<white-space>, <macro-terminator>)
  constant slot lexer-string :: <string> = " "; // not a mistake!
  constant slot parser-tag :: <symbol> = #"new-line";
end class;

// <pound> and <pound-pound> tokens are also discarded during macro
// expansion which is the only context in which they have meaning.

define class <pound> (<token>)
  constant slot lexer-string :: <string> = "#";
  constant slot parser-tag :: <symbol> = #"pound";
end class;

define class <pound-pound> (<token>)
  constant slot lexer-string :: <string> = "##";
  constant slot parser-tag :: <symbol> = #"pound-pound";
end class;

// Because of the separate name spaces for enum, union and struct tags and
// other names, tags are valid for names appearing either as <identifier>
// or as <typedef-name> tokens.  The class <identifier-or-typedef-name> is
// needed for dispatch on the rules which parse tags.
define abstract class <identifier-or-typedef-name> (<token>) end class;

define class <identifier> (<identifier-or-typedef-name>)
  slot lexer-string :: <string>, init-keyword: lexer-string:;
  constant slot parser-tag :: <symbol> = #"identifier";
end class;

// this method is correct only if it is called on tokens which have been
// fully macro expanded.  the rule here is that in #if compile time
// constant expression an identifier is given the value 0 if it isn't a
// defined macro.  therefore any identifier present in a fully expanded
// expression must have the value 0.

define sealed generic constant-value
    (token :: type-union(<identifier>, <integer-literal>,
                         <character-literal>))
 => (result :: <machine-word>);

define method constant-value (token :: <identifier>)
 => (result :: <machine-word>)
  as(<machine-word>, 0)
end method;

define method copy-token
    (source-token :: <identifier>,
     #key source-line: the-source-line :: false-or(<integer>))
 => (result-token :: <identifier>)
  let result-token = next-method();
  result-token.lexer-string := copy-sequence(source-token.lexer-string);
  result-token
end method;

define method token-print-string
    (the-token :: <identifier>) => (result :: <string>);
  concatenate(next-method(), " lexer-string: \"", the-token.lexer-string, "\"")
end method;

// <punctuation> tokens don't support either dylan-value or constant-value
define abstract class <punctuation> (<token>) end class;

define sealed generic precedence
    (token :: type-union(<symbol-token>, <punctuation>))
 => (result :: <integer>);

define constant <associativity-type> = one-of(#"right", #"left");

define sealed generic associativity
    (token :: type-union(<symbol-token>, <punctuation>))
 => (result :: <associativity-type>);

define sealed concrete class <open-parenthesis> (<punctuation>)
  constant slot lexer-string :: <string> = "(";
  constant slot parser-tag :: <symbol> = #"(";
  constant slot precedence :: <integer> = 0;
  constant slot associativity :: <associativity-type> = #"right";
end class;

define sealed concrete class <close-parenthesis> (<punctuation>)
  constant slot lexer-string :: <string> = ")";
  constant slot parser-tag :: <symbol> = #")";
  constant slot precedence :: <integer> = 0;
  constant slot associativity :: <associativity-type> = #"right";
end class;

define sealed concrete class <open-bracket> (<punctuation>)
  constant slot lexer-string :: <string> = "[";
  constant slot parser-tag :: <symbol> = #"[";
end class;

define sealed concrete class <close-bracket> (<punctuation>)
  constant slot lexer-string :: <string> = "]";
  constant slot parser-tag :: <symbol> = #"]";
end class;

define sealed concrete class <open-brace> (<punctuation>)
  constant slot lexer-string :: <string> = "{";
  constant slot parser-tag :: <symbol> = #"{";
end class;

define sealed concrete class <close-brace> (<punctuation>)
  constant slot lexer-string :: <string> = "}";
  constant slot parser-tag :: <symbol> = #"}";
end class;

define sealed concrete class <comma> (<punctuation>)
  constant slot lexer-string :: <string> = ",";
  constant slot parser-tag :: <symbol> = #",";
end class;

define sealed concrete class <semi-colon> (<punctuation>)
  constant slot lexer-string :: <string> = ";";
  constant slot parser-tag :: <symbol> = #";";
end class;

define class <ellipsis> (<punctuation>)
  constant slot lexer-string :: <string> = "...";
  constant slot parser-tag :: <symbol> = #"...";
end class;

define sealed concrete class <eoi> (<punctuation>, <macro-terminator>)
  constant slot lexer-string :: <string> = "";
  constant slot parser-tag :: <symbol> = #"eoi";
end class;

define constant $eoi-token = make(<eoi>);

define method quoted-string (token :: <eoi>) => (result :: <string>);
  error("attempt to quote the end of input token");
end method;

define abstract class <literal-token> (<token>)
end class;

define method copy-token
    (source-token :: <literal-token>,
     #key source-line: the-source-line :: false-or(<integer>) = #f)
 => (result-token :: <literal-token>)
  let result-token = next-method();
  result-token.lexer-string := copy-sequence(source-token.lexer-string);
  result-token
end method;

define method token-print-string
    (the-token :: <literal-token>) => (result :: <string>);
  concatenate(next-method(), " lexer-string: \"", the-token.quoted-string,
              "\"")
end method;

define sealed concrete class <character-literal> (<literal-token>)
  constant slot parser-tag :: <symbol> = #"character-literal";
  slot lexer-string :: <string>, init-keyword: lexer-string:;
end class;

// machine integer value for constant expression evaluation
define method constant-value
    (token :: <character-literal>) => (result :: <machine-word>);
  as(<machine-word>, as(<integer>, token.dylan-value))
end;

define method quoted-string
    (token :: <character-literal>) => (result :: <string>);
  let result = make(<stretchy-vector>);
  for(character in token.lexer-string)
    select(character)
      '"' => add!(result, '\\'); add!(result, '"');
      '\\' => add!(result, '\\'); add!(result, '\\');
      otherwise => add!(result, character);
    end select;
  end for;
  as(<string>, result)
end method;

define method dylan-value
    (token :: <character-literal>) => (result :: <character>);
  let index :: <integer> = 1; // skip the '\''
  let (result, new-index) =
    if (token.lexer-string[index] = '\\')
      escape-sequence-to-character(token.lexer-string, index + 1)
    else
      values(token.lexer-string[index], index + 1)
    end if;
  if (new-index + 1 > size(token.lexer-string))
    error("dylan-value: multi-byte characters not supported, input = %=",
          as(<string>, token.lexer-string));
  end;
  result
end method dylan-value;

//  escape scanner used by dylan value for both <character-literal> and
//  <string-literal>.
define method escape-sequence-to-character
    (input-string :: <sequence>, index :: <integer>)
  => (result :: <character>, index :: <integer>);
  if (clex-out-of-range-character?(input-string[index]))
    error("unexpected escape character in literal string %= ",
          input-string[index]);
  end if;
  let category = clex-escape-category(input-string[index]);
  case
    clex-hex-escape-category?(category)
      //  hex escapes -- "\x" followed by any number of hex digits
      => index := index + 1; // skip the 'x'
      let result = 0;
      while(clex-hex-escape-digit?(input-string[index]))
        result := result * 16 + clex-digit-to-integer(input-string[index]);
        index := index + 1;
      end while;
      values(as(<character>, result),
             index);
    clex-octal-escape-digit-category?(category)
      //  octal escapes --  "\" followed by 1 to 3 octal digits
      =>
      let result = clex-digit-to-integer(input-string[index]);
      index := index + 1;
      if (clex-octal-escape-digit?(input-string[index]))
        result := result * 8 + clex-digit-to-integer(input-string[index]);
        index := index + 1;
        if (clex-octal-escape-digit?(input-string[index]))
          result := result * 8 + clex-digit-to-integer(input-string[index]);
          index := index + 1;
        end if;
      end if;
      values(as(<character>, result),
             index);
    clex-character-escape-category?(category)
      =>
      values(character-escape-value(input-string[index]),
             index + 1);
    otherwise
      => error("unexpected escape character in literal string %= ",
               input-string[index]);
  end case;
end method escape-sequence-to-character;

define sealed concrete class <string-literal> (<literal-token>)
  constant slot parser-tag :: <symbol> = #"string-literal";
  slot internal-lexer-string-value :: type-union(<string>, <t-list>),
    init-keyword: lexer-string:;
end class;

define method lexer-string-setter(value, token :: <string-literal>)
  token.internal-lexer-string-value := value;
end method;

// Quoted string shouldn't ever be called on a lexer string value
// which isn't a <string> since expansion of # and ## should happen
// before adjacent strings are concatenated so there won't be any
// <t-list> beasties yet.
define method
    lexer-string(token :: <string-literal>) => (result :: <string>);
  select (token.internal-lexer-string-value by instance?)
    <string> => token.internal-lexer-string-value;
    <t-list> =>
      block()
        let list-of-strings =
          as(<list>, token.internal-lexer-string-value);
        let result = list-of-strings.head;
        for (another-string in list-of-strings.tail)
          result := concatenate(result, " ", another-string);
        end for;
        result
      end;
    otherwise =>
      error("unrecognized class for internal-lexer-string-value");
  end select;
end method;

// Quoted string shouldn't ever be called on a String literal where
// the internal-lexer-string-value isn't a <string> since expansion of
// # and ## should happen before adjacent strings are concatenated so
// there won't be any <t-list> beasties yet.

define method quoted-string
    (token :: <string-literal>) => (result :: <string>);
  let result = make(<stretchy-vector>);
  for(index from 0 below token.lexer-string.size)
    select(token.lexer-string[index])
      '"' => add!(result, '\\'); add!(result, '"');
      '\\' => add!(result, '\\'); add!(result, '\\');
      otherwise => add!(result, token.lexer-string[index]);
    end select;
  end for;
  as(<string>, result)
end method;

// Turn the parsed characters of a c string into a dylan string.  This
// method removes the surrounding double quotes and converts C escape
// sequences into <character>s. For internal lexer strings Which are
// really sequences (<t-list>s) of lists this concatenate the lists
// after doing the escape character expansion so that characters at
// the beginning of a concatenated string don't get subsumed into
// escape sequences at the end of a preceding string.

define method dylan-value
    (token :: <string-literal>) => (result :: <string>);
  let value-for-one-string =
    method(input-string, result)
        let index  = 1; // skip the '"'
        while(index < size(input-string) - 1)
          if (input-string[index] = '\\')
            let(new-character, new-index)
              = escape-sequence-to-character(input-string, index + 1);
            add!(result, new-character);
            index := new-index;
          else
            add!(result, input-string[index]);
            index := index + 1;
          end if;
        end while;
        result
    end method;
  let result = make(<stretchy-vector>);
  select (token.internal-lexer-string-value by instance?)
    <string> => value-for-one-string(token.internal-lexer-string-value, result);
    <t-list> =>
      for (string in token.internal-lexer-string-value)
        value-for-one-string(string, result);
      end for;
    otherwise => error("unexpected type for lexer-string");
  end select;
  as(<string>, result)
end method dylan-value;

define abstract primary class <include-filename> (<literal-token>)
  slot lexer-string :: <string>, init-keyword: lexer-string:;
end;

define sealed concrete class <ordinary-filename> (<include-filename>)
  constant slot parser-tag :: <symbol> = #"ordinary-filename";
end;

define sealed concrete class <standard-filename> (<include-filename>)
  constant slot parser-tag :: <symbol> = #"standard-filename";
end;

// remove the enclosing double quotes or angle brackets
define method dylan-value
    (token :: <include-filename>) => (result :: <string>);
  copy-sequence(token.lexer-string, start: 1,
                end: size(token.lexer-string) - 1)
end method dylan-value;

// need maybe unsigned/long discrimination for integer literals

define abstract class <integer-literal> (<literal-token>)
  constant slot parser-tag :: <symbol> = #"integer-literal";
  slot lexer-string :: <string>, init-keyword: lexer-string:;
end class;

define sealed concrete class
    <decimal-integer-literal> (<integer-literal>)
end;

// machine integer value for constant expression evaluation
define method constant-value
    (token :: <decimal-integer-literal>) => (result :: <machine-word>);
  as-decimal-machine-word(token.lexer-string)
end;

define method as-decimal-machine-word (string :: <sequence>)
    => (number :: <machine-word>);
  let number :: <machine-word> = clex-digit-to-machine-word(string[0]);
  let start-index =  1;
  for (i from start-index below string.size)
    number :=
      number * as(<machine-word>, 10) + clex-digit-to-machine-word(string[i]);
  end for;
  number
end method;

define method dylan-value
    (token :: <decimal-integer-literal>) => (result :: stupid-<integer>);
  as-decimal-integer(token.lexer-string)
end;

define method as-decimal-integer (string :: <sequence>)
    => (number :: stupid-<integer>)
  let number :: stupid-<integer> = clex-digit-to-integer(string[0]);
  let start-index =  1;
  for (i from start-index below string.size)
    number :=
      stupid-+(stupid-*(number, 10), clex-digit-to-integer(string[i]));
  end for;
  number
end method;

define sealed concrete class
    <octal-integer-literal> (<integer-literal>)
end;

// machine integer value for constant expression evaluation
define method constant-value
    (token :: <octal-integer-literal>) => (result :: <machine-word>);
  as-octal-machine-word(token.lexer-string)
end;

define method as-octal-machine-word (string :: <sequence>)
    => (number :: <machine-word>);
  let number :: <machine-word> = clex-digit-to-machine-word(string[0]);
  let start-index =  1;
  for (i from start-index below string.size)
    number :=
      number * as(<machine-word>, 8) + clex-digit-to-machine-word(string[i]);
  end for;
  number
end method;

define method dylan-value
    (token :: <octal-integer-literal>) => (result :: stupid-<integer>);
  as-octal-integer(token.lexer-string)
end;

define method as-octal-integer (string :: <sequence>)
    => (number :: stupid-<integer>);
  let number :: stupid-<integer> = clex-digit-to-integer(string[0]);
  let start-index =  1;
  for (i from start-index below string.size)
    number :=
      stupid-+(stupid-*(number, 8), clex-digit-to-integer(string[i]));
  end for;
  number
end method;

define sealed concrete class
    <hexadecimal-integer-literal> (<integer-literal>)
end;

// machine integer value for constant expression evaluation
define method constant-value
    (token :: <hexadecimal-integer-literal>) => (result :: <machine-word>);
  as-hexadecimal-machine-word(token.lexer-string)
end;

define method as-hexadecimal-machine-word (string :: <sequence>)
    => int :: <machine-word>;
  let number :: <machine-word> = clex-digit-to-machine-word(string[0]);
  let start-index =  1;
  for (i from start-index below string.size)
    number :=
      number * as(<machine-word>, 16) + clex-digit-to-machine-word(string[i]);
  end for;
  number
end method;

// machine integer value for constant expression evaluation
define method dylan-value
    (token :: <hexadecimal-integer-literal>) => (result :: stupid-<integer>);
  as-hexadecimal-integer(token.lexer-string)
end;

define method as-hexadecimal-integer (string :: <sequence>)
    => (number :: stupid-<integer>);
  let number :: stupid-<integer> = clex-digit-to-integer(string[0]);
  let start-index =  1;
  for (i from start-index below string.size)
    number :=
      stupid-+(stupid-*(number, 16), clex-digit-to-integer(string[i]));
  end for;
  number
end method;

define concrete class <float-literal> (<literal-token>)
  constant slot parser-tag :: <symbol> = #"float-literal";
  slot lexer-string :: <string>, init-keyword: lexer-string:;
  slot has-exponent? :: <boolean>,
    required-init-keyword: has-exponent?:;
end class;

define method dylan-value
    (token :: <float-literal>) => (result :: <float>);
  let string :: <string> = token.lexer-string;
  // strip leading zeros
  let i = 0;
  while(string[i] == '0') i := i + 1; end while;
  if (i > 0) string := copy-sequence(string, start: i); end if;
  i :=  string.size - 1;
  let float-size = #"double";
  let exponent = 0;
  if (token.has-exponent?)
    // collect the exponent as a decimal integer
    let multiplier = 1;
    let done = #f;
    until(done)
      select (string[i] by \==)
        'e', 'E' => done := #t;
        '+', 'l', 'L' => #f; // do nothing, just skip it
        'f', 'F' => float-size := #"single";
        '-' => exponent := exponent * -1;
        otherwise =>
          exponent :=
            exponent + (clex-digit-to-integer(string[i]) * multiplier);
          multiplier := multiplier * 10;
      end select;
      i := i - 1;
    end until;
  end if;
  // Now collect the constant together with number of digits left of the
  // decimal point so we can convert to a normalized float (all digits
  // to the right of the decimal point).
  let constant-part = 0.d0;
  let digits-to-the-left = 0;
  until (i < 0)
    select (string[i] by \==)
      '.' => digits-to-the-left := 0; // Everything so far was to the right
      'l', 'L' => #f; // do nothing, just skip it
      'f', 'F' => float-size := #"single";
      otherwise =>
        constant-part :=
          (constant-part + clex-digit-to-integer(string[i])) * 0.1;
        digits-to-the-left := digits-to-the-left + 1;
    end select;
    i := i - 1;
  end until;
  exponent := exponent + digits-to-the-left;
  let result = constant-part * (10.d0 ^ exponent);
  if (float-size == #"single") as(<single-float>, result) else result end
end method;

// no method for constant-value of a float literal needed since floats can't be
// part of constants in #if conditions.

define abstract class <symbol-token> (<token>) end class;

// at this point the only symbol tokens identified as either unary or
// binary operators are those which can be used in compile time constant
// expressions in #if cpp directives.

define abstract class <unary-operator> (<symbol-token>) end class;
define abstract class <binary-operator> (<symbol-token>) end class;

define method lexer-string
    (token :: <symbol-token>) => (result :: <string>);
  as(<string>, token.parser-tag)
end method;

define sealed concrete class <dot-star> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #".*";
end;

define sealed concrete class <dot> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #".";
end;

define sealed concrete class <and-and> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"&&";
  constant slot precedence :: <integer> =  5;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <and-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"&=";
end;

define sealed concrete class <and> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"&";
  constant slot precedence :: <integer> = 8;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <multiply-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"*=";
end;

define sealed concrete class <star> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"*";
  constant slot precedence :: <integer> = 13;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <plus-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"+=";
end;

define sealed concrete class <plus> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"+";
  constant slot precedence :: <integer> = 12;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <unary-plus> (<unary-operator>)
  constant slot parser-tag :: <symbol> = #"+";
  constant slot precedence :: <integer> = 15;
  constant slot associativity :: <associativity-type> = #"right";
end;

define sealed concrete class <plus-plus> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"++";
end;

define sealed concrete class <minus-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"-=";
end;

define sealed concrete class <minus> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"-";
  constant slot precedence :: <integer> = 12;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <unary-minus> (<unary-operator>)
  constant slot parser-tag :: <symbol> = #"-";
  constant slot precedence :: <integer> = 15;
  constant slot associativity :: <associativity-type> = #"right";
end;

define sealed concrete class <minus-minus> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"--";
end;

define sealed concrete class <arrow> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"->";
end;

define sealed concrete class <arrow-star> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"->*";
end;

define sealed concrete class <tilde> (<unary-operator>)
  constant slot parser-tag :: <symbol> = #"~";
  constant slot precedence :: <integer> = 15;
  constant slot associativity :: <associativity-type> = #"right";
end;

define sealed concrete class <not-equal> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"!=";
  constant slot precedence :: <integer> = 9;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <not> (<unary-operator>)
  constant slot parser-tag :: <symbol> = #"!";
  constant slot precedence :: <integer> = 15;
  constant slot associativity :: <associativity-type> = #"right";
end;

define sealed concrete class <divide-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"/=";
end;

define sealed concrete class <divide> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"/";
  constant slot precedence :: <integer> = 13;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <remainder-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"%=";
end;

define sealed concrete class <remainder> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"%";
  constant slot precedence :: <integer> = 13;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <left-shift-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"<<=";
end;

define sealed concrete class <left-shift> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"<<";
  constant slot precedence :: <integer> = 11;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <less-than-or-equal> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"<=";
  constant slot precedence :: <integer> = 10;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <less-than> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"<";
  constant slot precedence :: <integer> = 10;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <right-shift-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #">>=";
end;

define sealed concrete class <right-shift> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #">>";
  constant slot precedence :: <integer> = 11;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <greater-than-or-equal> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #">=";
  constant slot precedence :: <integer> = 10;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <greater-than> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #">";
  constant slot precedence :: <integer> = 10;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <carat-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"^=";
end;

define sealed concrete class <carat> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"^";
  constant slot precedence :: <integer> = 7;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <or-assign> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"|=";
end;

define sealed concrete class <or> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"|";
  constant slot precedence :: <integer> = 6;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <or-or> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"||";
  constant slot precedence :: <integer> = 4;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <equal-equal> (<binary-operator>)
  constant slot parser-tag :: <symbol> = #"==";
  constant slot precedence :: <integer> = 9;
  constant slot associativity :: <associativity-type> = #"left";
end;

define sealed concrete class <equal> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"=";
end;

define sealed concrete class <question> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"?";
  constant slot precedence :: <integer> = 3;
  constant slot associativity :: <associativity-type> = #"right";
end;

define sealed concrete class <colon> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #":";
  constant slot precedence :: <integer> = 3;
  constant slot associativity :: <associativity-type> = #"right";
end;

define sealed concrete class <colon-colon> (<symbol-token>)
  constant slot parser-tag :: <symbol> = #"::";
end;

// reserved word tokens.

define open abstract class <reserved-word> (<token>) end;

define method lexer-string
    (token :: <reserved-word>) => (result :: <string>);
  as(<string>, token.parser-tag)
end method;


// <linkage-specifier>s are Microsoft specific extensions. They are
// documented as "attributes" in the Microsoft BNF but in the code they
// appear as modifiers to function declarators immediately preceding the
// declarator for the function and following any stars which indicate
// pointer to function types.

define abstract class <linkage-specifier> (<reserved-word>) end;

define sealed concrete class <__fastcall> (<linkage-specifier>)
  constant slot parser-tag :: <symbol> = #"__fastcall";
end;

// Also they use _cdecl end __cdecl interchangeably
define sealed concrete class <__cdecl> (<linkage-specifier>)
  constant slot parser-tag :: <symbol> = #"__cdecl";
end;

define sealed concrete class <_cdecl> (<linkage-specifier>)
  constant slot parser-tag :: <symbol> = #"__cdecl";
end;

define sealed concrete class <__stdcall> (<linkage-specifier>)
  constant slot parser-tag :: <symbol> = #"__stdcall";
end;

define abstract class <microsoft-attribute> (<reserved-word>) end;

// Yes Virginia, they really use both _inline and __inline
// interchangeably and they didn't even #define one to the other.
// Note that the parser-tags are the same even though the classes and
// lexer-strings are different.
define sealed concrete class <_inline> (<Microsoft-attribute>)
  constant slot lexer-string :: <string> = "_inline";
  constant slot parser-tag :: <symbol> = #"_inline";
end;

define sealed concrete class <__inline> (<Microsoft-attribute>)
  constant slot lexer-string :: <string> = "__inline";
  constant slot parser-tag :: <symbol> = #"_inline";
end;


define sealed concrete class <__asm> (<microsoft-attribute>)
  constant slot parser-tag :: <symbol> = #"__asm";
end;

define sealed concrete class <__based> (<microsoft-attribute>)
  constant slot parser-tag :: <symbol> = #"__based";
end;

define abstract primary class <storage-class> (<reserved-word>) end;
define abstract primary class <microsoft-storage-class> (<storage-class>) end;
define abstract primary class <ansi-storage-class> (<microsoft-storage-class>) end;

define sealed concrete class <auto> (<ansi-storage-class>)
  constant slot parser-tag :: <symbol> = #"auto";
end;

define sealed concrete class <register> (<ansi-storage-class>)
  constant slot parser-tag :: <symbol> = #"register";
end;

define sealed concrete class <static> (<ansi-storage-class>)
  constant slot parser-tag :: <symbol> = #"static";
end;

define sealed concrete class <extern> (<ansi-storage-class>)
  constant slot parser-tag :: <symbol> = #"extern";
end;

define sealed concrete class <typedef> (<ansi-storage-class>)
  constant slot parser-tag :: <symbol> = #"typedef";
end;

define sealed concrete class <__declspec> (<microsoft-storage-class>)
  constant slot parser-tag :: <symbol> = #"__declspec";
end;


//  extended-decl-modifier -- microsoft specific
// these are context sensitive reserved words so they are just identifiers
// during parsing.
//
// extended-decl-modifier :
//         thread
//         naked
//         dllimport
//         dllexport

define abstract class <type-specifier> (<reserved-word>) end;
define abstract class <microsoft-type-specifier> (<type-specifier>) end;
define abstract class <ansi-type-specifier> (<microsoft-type-specifier>) end;

define sealed concrete class <void> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"void";
end;

define sealed concrete class <char> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"char";
end;

define sealed concrete class <short> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"short";
end;

define sealed concrete class <int> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"int";
end;

define sealed concrete class <__int8> (<microsoft-type-specifier>)
  constant slot parser-tag :: <symbol> = #"__int8";
end;

define sealed concrete class <__int16> (<microsoft-type-specifier>)
  constant slot parser-tag :: <symbol> = #"__int16";
end;

define sealed concrete class <__int32> (<microsoft-type-specifier>)
  constant slot parser-tag :: <symbol> = #"__int32";
end;

define sealed concrete class <__int64> (<microsoft-type-specifier>)
  constant slot parser-tag :: <symbol> = #"__int64";
end;

define sealed concrete class <long> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"long";
end;

// can't use <float> because it conflicts with the dylan class
define sealed concrete class <c-float-type> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"float";
end;

define sealed concrete class <double> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"double";
end;

define sealed concrete class <signed> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"signed";
end;

define sealed concrete class <unsigned> (<ansi-type-specifier>)
  constant slot parser-tag :: <symbol> = #"unsigned";
end;

define abstract class <type-qualifier> (<reserved-word>) end;


define abstract class <Microsoft-type-qualifier> (<type-qualifier>) end;
define abstract class <ansi-type-qualifier> (<Microsoft-type-qualifier>) end;

// __unaligned is a reserved word only in certain versions of the Microsoft C
// compilers.  Usually they chop up the headers with conditional
// compilation.  I am assuming that it is reserved for all Microsoft header
// files.  That might be a problem but only if somebody defines __unaligned
// as a macro or typedef.  They deserve what they get if they do that.
define sealed concrete class <__unaligned> (<Microsoft-type-qualifier>)
  constant slot parser-tag :: <symbol> = #"__unaligned";
end;

define sealed concrete class <const> (<ansi-type-qualifier>)
  constant slot parser-tag :: <symbol> = #"const";
end;

define sealed concrete class <volatile> (<ansi-type-qualifier>)
  constant slot parser-tag :: <symbol> = #"volatile";
end;

define abstract class <struct-or-union> (<reserved-word>) end;

define sealed concrete class <struct> (<struct-or-union>)
  constant slot parser-tag :: <symbol> = #"struct";
end;

define sealed concrete class <union> (<struct-or-union>)
  constant slot parser-tag :: <symbol> = #"union";
end;

define sealed concrete class <enum> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"enum";
end;

define sealed concrete class <break> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"break";
end;

define sealed concrete class <case> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"case";
end;

define sealed concrete class <continue> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"continue";
end;

define sealed concrete class <default> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"default";
end;

define sealed concrete class <do> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"do";
end;

define sealed concrete class <else> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"else";
end;

define sealed concrete class <for> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"for";
end;

define sealed concrete class <goto> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"goto";
end;

define sealed concrete class <if> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"if";
end;

define sealed concrete class <return> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"return";
end;

define sealed concrete class <sizeof> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"sizeof";
end;

define sealed concrete class <switch> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"switch";
end;

define sealed concrete class <while> (<reserved-word>)
  constant slot parser-tag :: <symbol> = #"while";
end;

define sealed concrete class <typedef-name> (<identifier-or-typedef-name>)
  slot lexer-string :: <string>, init-keyword: lexer-string:;
  constant slot parser-tag :: <symbol> = #"typedef-name";
end;

define method copy-token
    (source-token :: <typedef-name>,
     #key source-line: the-source-line :: false-or(<integer>))
 => (result-token :: <typedef-name>)
  let result-token = next-method();
  result-token.lexer-string := copy-sequence(source-token.lexer-string);
  result-token
end method;

define method token-print-string
    (the-token :: <typedef-name>) => (result :: <string>);
  concatenate(next-method(), " lexer-string: \"", the-token.lexer-string, "\"")
end method;

