Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method convert-const-exp (expression :: <ast-expression>)
 => (string :: <string>)

  convert-literal(expression-value(expression));

/*
  let combinator = expression-combinator(expression);
  select (combinator by instance?)
    <no-combinator>
      => convert-literal(expression-value(expression));

    <symbol-combinator>
      => map-to-dylan-constant-name(expression-scoped-name(expression));

    <unary-expression-combinator>
      => format-to-string("%s(%s)",
			  convert-combinator(combinator),
			  convert-const-exp(right-subexpression(expression)));

    <binary-expression-combinator>
      => format-to-string("%s(%s, %s)",
			  convert-combinator(combinator),
			  convert-const-exp(left-subexpression(expression)),
			  convert-const-exp(right-subexpression(expression)));

    otherwise
      => error("convert-const-exp: unrecognised combinator.");
  end select;
*/
end method;


//
// CONVERT-LITERAL
//

define generic convert-literal (literal) => (string :: <string>);

define method convert-literal (literal)
 => (string :: <string>)
  format-to-string("%=", literal);
end method;

define method convert-literal (string :: <string>)
 => (string :: <string>)
  let index = 0;
  let result = make(<stretchy-vector>);
  while(index < size(string))
    if (string[index] = '\\')
      let(new-substring, new-index)
	= convert-string-escape-sequence(string, index + 1);
      do(curry(add!, result), new-substring);
      index := new-index;
    else
      add!(result, string[index]);
      index := index + 1;
    end if;
  end while;
  as(<string>, result)
end method;

define method convert-string-escape-sequence (string :: <string>, index :: <integer>)
 => (result :: <string>, new-index :: <integer>)
  case
    string[index] = 'v'
      => values("\\<0B>", index + 1);

    string[index] = '?'
      => values("?", index + 1);

    octal-digit?(string[index])
      => begin
	   let number = octal-digit-to-integer(string[index]);
	   index := index + 1;
	   if (index < size(string) & octal-digit?(string[index]))
	     number := number * 8 + octal-digit-to-integer(string[index]);
	     index := index + 1;
	     if (index < size(string) & octal-digit?(string[index]))
	       number := number * 8 + octal-digit-to-integer(string[index]);
	       index := index + 1;
	     end if;
	   end if;
	   values(format-to-string("\\<%x>", number), index);
	 end;

    string[index] = 'x'
      => begin
	   let finish = index + 1;
	   while (finish < size(string) & hex-digit?(string[finish]))
	     finish := finish + 1;
	   end while;
	   let result = make(<string>, size: finish - index + 2, fill: '\\');
	   result[1] := '<';
	   for (i from index + 1 below finish)
	     result[i - index + 1] := string[i];
	   end for;
	   result[finish - index + 1] := '>';
	   values(result, finish);
	 end;

    otherwise
      => begin
	   let result = make(<string>, size: 2, fill: '\\');
	   result[1] := string[index];
	   values(result, index + 1);
	 end;
  end case;
end method;


define method hex-digit? (c :: <character>)
 => (well? :: <boolean>)
  (((c >= 'a') & (c <= 'f'))
     | ((c >= 'A') & (c <= 'F'))
     | ((c >= '0') & (c <= '9')))
end method;

define method octal-digit? (c :: <character>)
 => (well? :: <boolean>)
  (c >= '0') & (c <= '8')
end method;

define method octal-digit-to-integer (digit :: <character>)
 => (number :: <integer>)
  as(<integer>, digit) - as(<integer>, '0')
end method;



//
// CONVERT-COMBINATOR
//
// TODO: Maybe use a table instead of all these methods
//
/*
define generic convert-combinator (combinator :: <expression-combinator>)
 => (string :: <string>);

define method convert-combinator (combinator :: <unary-plus-combinator>)
 => (string :: <string>)
  "identity";
end method;

define method convert-combinator (combinator :: <unary-minus-combinator>)
 => (string :: <string>)
  "negative";
end method;

define method convert-combinator (combinator :: <bitwise-negation-combinator>)
 => (string :: <string>)
  "lognot";
end method;

define method convert-combinator (combinator :: <add-combinator>)
 => (string :: <string>)
  "\\+";
end method;

define method convert-combinator (combinator :: <minus-combinator>)
 => (string :: <string>)
  "\\-";
end method;

define method convert-combinator (combinator :: <multiply-combinator>)
 => (string :: <string>)
  "\\*";
end method;

define method convert-combinator (combinator :: <divide-combinator>)
 => (string :: <string>)
  "\\/";
end method;

define method convert-combinator (combinator :: <modulus-combinator>)
 => (string :: <string>)
  "modulo";
end method;

define method convert-combinator (combinator :: <or-combinator>)
 => (string :: <string>)
  "logior";
end method;

define method convert-combinator (combinator :: <xor-combinator>)
 => (string :: <string>)
  "logxor";
end method;

define method convert-combinator (combinator :: <and-combinator>)
 => (string :: <string>)
  "logand";
end method;

define method convert-combinator (combinator :: <left-shift-combinator>)
 => (string :: <string>)
  "ash";
end method;

define method convert-combinator (combinator :: <right-shift-combinator>)
 => (string :: <string>)
  "method (n, s) ash(n, -s) end method";
end method;
*/
