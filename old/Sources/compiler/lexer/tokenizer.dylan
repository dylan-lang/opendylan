Module:   lexer
Synopsis: Tokenization routines for recognized buffer text
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method tokenize-symbolic (s :: <lexer-state>)
  if (s.buf[s.buf-ptr - 1] == ':' & s.buf[0] ~== ':') // Hack!!!
    values($keyword, as(<symbol>, copy-sequence(s.buf, end: s.buf-ptr - 1)))
  else
    s.classifier(as(<symbol>, copy-sequence(s.buf, end: s.buf-ptr)))
  end
end method;

define method tokenize-symbol (s :: <lexer-state>)
  values ($literal, 
          as(<symbol>, copy-sequence(s.buf, start: 2, end: s.buf-ptr - 1)))
end method;

define method tokenize-string (s :: <lexer-state>)
  values($string, copy-sequence(s.buf, start: 1, end: s.buf-ptr - 1))
end method;

define method tokenize-character (s :: <lexer-state>)
  values($literal, s.buf[1])
end method;

define method tokenize-punctuation (s :: <lexer-state>)
  let name-string :: <string> = copy-sequence(s.buf, end: s.buf-ptr);
  let name :: <symbol> = as(<symbol>, name-string);
  select (name)
    #"(" => values($lbracket, name);
    #")" => values($rbracket, name);
    #"," => values($var-sep, name);
    #"." => values($dot, name);
    #";" => values($statement-sep, name);
    #"[" => values($lsbracket, name);
    #"]" => values($rsbracket, name);
    #"{" => values($lcbracket, name);
    #"}" => values($rcbracket, name);
    #"#(" => values($list-open, name);
    #"#[" => values($vector-open, name);
  end
end method;

define method tokenize-binary-integer (s :: <lexer-state>)
  tokenize-any-integer(s, 2, 2);
end method;

define method tokenize-octal-integer (s :: <lexer-state>)
  tokenize-any-integer(s, 2, 8);
end method;

define method tokenize-decimal-integer (s :: <lexer-state>)
  select (s.buf[0])
    '+' => tokenize-any-integer(s, 1, 10);
    '-' => tokenize-any-integer(s, 1, 10, sign: -1);
    otherwise => tokenize-any-integer(s, 0, 10);
  end
end method;

define method tokenize-hex-integer (s :: <lexer-state>)
  tokenize-any-integer(s, 2, 16);
end method;

define method tokenize-any-integer 
    (s :: <lexer-state>, start :: <integer>, base :: <integer>, 
     #key sign :: <integer> = 1)
  let count :: <integer> = 0;
  let ptr :: <integer> = s.buf-ptr;
  for (i :: <integer> from start below ptr)
    let char :: <character> = s.buf[i];
    let value :: <integer> = digit-value(char);
    count := count * base + value;
  end;
  values($literal, sign * count)
end method;

define method tokenize-floating-point (s :: <lexer-state>)
  values($literal, 99.9)
end method;

define method tokenize-hash-token (s :: <lexer-state>)
  s.buf[0] := '&';
  let name = as(<symbol>, copy-sequence(s.buf, end: s.buf-ptr));
  select (name)
    #"&t" => values($literal, #t);
    #"&f" => values($literal, #f);
    otherwise
      values(name, name)
  end
end method;

// Utils:

define constant $zero-code = as(<integer>, '0');
define constant $nine-code = as(<integer>, '9');
define constant $a-code = as(<integer>, 'a');
define constant $z-code = as(<integer>, 'z');

define method digit-value (c :: <character>)
  let c-code = as(<integer>, as-lowercase(c));
  case
    $zero-code <= c-code & c-code <= $nine-code
      => c-code - $zero-code;
    $a-code <= c-code & c-code <= $z-code
      => c-code - $a-code + 10;
  end
end method;

// eof
