Module:   lexer
Synopsis: The recognizer component of a Dylan lexer. This is currently
          hand-crafted pending the arrival of suitable lexer-generator
          technology. Hopefully just this component will require
          replacement.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Character classes from the lexical grammar specification:

// Cheat by making : a simple graphic character

define character-class alphabetic-character 
  = "abcdefghijklmnopqrstuvwxyz";

define character-class numeric-character
  = "0123456789";

define character-class graphic-character
  = "!&*<=>|^$%@_:";

define character-class special-character
  = "-+~?/";

define character-class any-character
  = (alphabetic-character, 
     numeric-character, 
     graphic-character, 
     special-character);

define character-class binary-digit
  = "01";

define character-class octal-digit
  = "01234567";

define character-class decimal-digit
  = "0123456789";

define character-class hex-digit
  = "0123456789abcdef";

define character-class equal-character
  = "=";

define character-class whitespace-character 
  = " \t\n";

define character-class simple-punctuation 
  = "(),;[]{}";

//// Top level actions:

define lexer-table *top*;

define lexer-action *top* on (whitespace-character)
  // Don't want to buffer really
  lex-while(s, whitespace-character?);
  lex(s)
end lexer-action;

define lexer-action *top* on ("/")
  let next :: <character> = lex-next(s);
  select (next)
    '/' 
      => lex-line-comment(s);
         lex(s);
    '*' 
      => lex-area-comment(s);
         lex(s);
    otherwise
      => unlex-next(s, next);
         lex-while(s, any-character?);
         tokenize-symbolic(s)
  end
end lexer-action;

define lexer-action *top* on (alphabetic-character, graphic-character)
  lex-while(s, any-character?);
  tokenize-symbolic(s);
end lexer-action;

define lexer-action *top* on ("~")
  lex-while(s, equal-character?);
  tokenize-symbolic(s)
end lexer-action;

define lexer-action *top* on (numeric-character, "+-.")
  unlex-next(s, s.buf[0]);
  reset-buffer!(s);
  lex-in-context(s, *maybe-number*)
end lexer-action;

define lexer-action *top* on ("\"")
  lex-string(s);
  tokenize-string(s)
end lexer-action;

define lexer-action *top* on ("\'")
  lex-character(s)
end lexer-action;

define lexer-action *top* on ("#")
  lex-in-context(s, *hash-token*)
end lexer-action;

define lexer-action *top* on (simple-punctuation)
  tokenize-punctuation(s)
end lexer-action;

define lexer-action *top* on ("\\")
  reset-buffer!(s);
  let (class, value) = lex-in-context(s, *top*);
  values($symbol, value)
end lexer-action;

//// #-token actions

define lexer-table *hash-token*;

define lexer-action *hash-token* on ("\"")
  lex-string(s);
  tokenize-symbol(s)
end lexer-action;

define lexer-action *hash-token* on ("(")
  tokenize-punctuation(s)
end lexer-action;

define lexer-action *hash-token* on ("[")
  tokenize-punctuation(s)
end lexer-action;

// This action is overridden below for 'b', 'o', and 'x'.
define lexer-action *hash-token* on (alphabetic-character, graphic-character)
  lex-while(s, any-character?);
  tokenize-hash-token(s)
end lexer-action;

define lexer-action *hash-token* on ("b")
  lex-while(s, binary-digit?);
  tokenize-binary-integer(s)
end lexer-action;

define lexer-action *hash-token* on ("o")
  lex-while(s, octal-digit?);
  tokenize-octal-integer(s)
end lexer-action;

define lexer-action *hash-token* on ("x")
  lex-while(s, hex-digit?);
  tokenize-hex-integer(s)
end lexer-action;

//// Number/symbol disambiguation

define lexer-table *maybe-number*;

define lexer-action *maybe-number* on (".")
  lex-while(s, decimal-digit?);
  if (s.buf-ptr = 1)
    tokenize-punctuation(s)
  else
    lex-exponent(s);
    tokenize-floating-point(s)
  end;
end lexer-action;

define lexer-action *maybe-number* on ("+-")
  let next :: <character> = lex-next(s);
  if (decimal-digit?(next) | next == '.')
    unlex-next(s, next);
    lex-maybe-number(s)
  else
    unlex-next(s, next);
    tokenize-symbolic(s)
  end
end lexer-action;

define lexer-action *maybe-number* on (decimal-digit)
  lex-maybe-number(s)
end lexer-action;

define method lex-maybe-number (s)
  lex-while(s, decimal-digit?);
  let next :: <character> = lex-next(s);
  select (next)
    'e', 'E'
      => unlex-next(s, next);
         lex-exponent(s);
         tokenize-floating-point(s);
    '.' 
      => buffer!(s, next);
         lex-while(s, decimal-digit?);
         lex-exponent(s);
         tokenize-floating-point(s);
    otherwise
      => if (alphabetic-character?(next))
           buffer!(s, next);
           lex-while(s, any-character?);
           tokenize-symbolic(s)
         else
           unlex-next(s, next);
           tokenize-decimal-integer(s)
         end
  end;
end method;

define method lex-exponent (s :: <lexer-state>)
  let next :: <character> = lex-next(s);
  if (next == 'e' | next == 'E')
    buffer!(s, next);
    lex-while(s, decimal-digit?)
  else
    unlex-next(s, next)
  end
end method;

//// Driver:

define method lex (s :: <lexer-state>)
  reset-buffer!(s);
  let next :: <character> = lex-next(s);
  if (as(<integer>, next) == -1)
    values($end-of-input, $end-of-input)
  else
    unlex-next(s, next);
    lex-in-context(s, *top*)
  end
end method;

define method dylan-lex (s :: <lexer-state>)
  lex(s)
end method;

define method lex-in-context 
    (s :: <lexer-state>, context :: <simple-object-vector>)
  let next :: <character> = lex-next-buffering(s);
  context[as(<integer>, next)](s)
end method;

// Custom lexers:

define method lex-string (s :: <lexer-state>)
  while (lex-constituent-character(s) ~== '"') end;
end method;

define method lex-character (s :: <lexer-state>)
  if (lex-constituent-character(s) == '\'')
    error("Bad character syntax");
  end;
  if (lex-next-buffering(s) ~== '\'')
    error("Bad character syntax");
  end;
  tokenize-character(s)
end method;

define method lex-constituent-character (s :: <lexer-state>)
  let next :: <character> = lex-next(s);
  select (next)
    '\\' => lex-escape-character(s);
    otherwise
      buffer!(s, next);
      next
  end;
end method;

define method lex-escape-character (s :: <lexer-state>)
  let next :: <character> = lex-next(s);
  buffer!(s,
    select (next)
      'a' => '\a';
      'b' => '\b';
      'e' => '\e';
      'f' => '\f';
      'n' => '\n';
      'r' => '\r';
      't' => '\t';
      '0' => '\0';
      '<' => lex-unicode-character(s);
      otherwise
        => next
    end);
end method;

define method lex-unicode-character (s :: <lexer-state>)
  let count :: <integer> = 0;
  for (next :: <character> = lex-next(s) then lex-next(s),
       until: next == '>')
    count := count * 16 + digit-value(next)
  end;
  as(<character>, count)
end method;

define method lex-line-comment (s :: <lexer-state>)
  while (lex-next(s) ~== '\n') end;
end method;

define method lex-area-comment (s :: <lexer-state>)
  block (return)
    let last = #f;
    for (next :: <character> = lex-next(s) then lex-next(s))
      select (next)
        '*'
          => if (last == '/')
               lex-area-comment(s);
               last := #f;
             else
               last := next;
             end;
        '/'
          => if (last == '/')
               lex-line-comment(s);
               last := #f;
             elseif (last == '*')
               return();
             else
               last := next
             end;
         otherwise
           => last := #f;
      end;
    end;
  end;
end method;

// eof
