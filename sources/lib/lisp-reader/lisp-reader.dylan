module: lisp-reader
synopsis: 
author: 
copyright: 

define constant $whitespace = one-of('\t', ' ', '\n', '\r');
define constant $end-of-symbol = type-union($whitespace, singleton(')'));

define function skip-whitespace (stream :: <stream>) => ();
  if (~ stream-at-end?(stream))
    let el = peek(stream);
    if (instance?(el, $whitespace))
      read-element(stream);
      skip-whitespace(stream);
    end;
  end;
end;

define function read-lisp (stream :: <stream>) => (result);
  skip-whitespace(stream);
  let ele = peek(stream);
  let result =
    select (ele)
      '(' => read-s-expression(stream);
      '"' => read-string(stream);
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
        => read-number(stream);
      otherwise => read-symbol(stream);
    end;
  result;
end;

define function read-s-expression (stream :: <stream>) => (result :: <list>);
  read-element(stream); // consume '('
  let result = #();
  skip-whitespace(stream);
  while (peek(stream) ~== ')')
    result := pair(read-lisp(stream), result);
  end;
  read-element(stream); // consume ')'
  reverse(result);
end;

define function read-string (stream :: <stream>) => (result :: <string>)
  read-element(stream); // consume "
  let result = "";
  skip-whitespace(stream);
  while (peek(stream) ~== '"')
    let c = read-element(stream);
    if (c == '\\')
      c := read-element(stream);
      c := select(c)
             't' => '\t';
             'r' => '\r';
             'n' => '\n';
             otherwise => c;
           end;
    end;
    result := add!(result, c);
  end;
  read-element(stream); // consume "
  result;
end;

define function read-word (stream :: <stream>) => (result :: <string>)
  let result = "";
  skip-whitespace(stream);
  while (~stream-at-end?(stream) & ~instance?(peek(stream), $end-of-symbol))
    result := add!(result, read-element(stream));
  end;
  result;
end;

define function read-number (stream :: <stream>) => (result :: <integer>)
  string-to-integer(read-word(stream));
end;

define function read-symbol (stream :: <stream>) => (result :: <symbol>)
  as(<symbol>, read-word(stream));
end;

define method print-s-expression (stream :: <stream>, expression :: <integer>)
  format(stream, "%d", expression);
end;

define method print-s-expression (stream :: <stream>, expression :: <symbol>)
  format(stream, "%s", expression);
end;

define method print-s-expression (stream :: <stream>, expression :: <string>)
  format(stream, "%=", expression);
end;

define method print-s-expression (stream :: <stream>, expression :: <collection>)
  format(stream, "(");
  for (e in expression)
    print-s-expression(stream, e);
    format(stream, " ");
  end;
  format(stream, ")");  
end;

