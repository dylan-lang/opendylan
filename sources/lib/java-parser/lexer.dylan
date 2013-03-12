Module: java-parser
Author: Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// should this inherit from <expression> as well?
define abstract class <variable-initializer> (<grammar-object>) end;
define abstract class <expression> (<variable-initializer>) end;
define abstract class <name> (<expression>) end;
define abstract class <block-statement> (<grammar-object>) end;
define abstract class <statement> (<block-statement>) end;
define abstract class <statement-expression> (<statement>) end;
define abstract class <type-descriptor> (<grammar-object>) end;

define abstract class <token> (<grammar-object>) end;
define generic token-parse-class (x :: <token>);


define grammar-sequence <statement-expressions> (<statement-expression>);
define grammar-sequence <block-statements> (<block-statement>);
define constant <statements> = limited(<vector>, of: <statement>);
define grammar-sequence <variable-initializers> (<variable-initializer>);
define grammar-sequence <expressions> (<expression>);
define grammar-sequence <names> (<name>);

// Literals
define abstract class <literal> (<token>, <expression>) end;

define method token-parse-class (x :: <literal>) => (parse-class)
  $literal-token
end;

define generic literal-value (x :: <literal>) => (value);

define abstract class <boolean-literal> (<literal>) end;
define class <true-literal> (<boolean-literal>) end;
define class <false-literal> (<boolean-literal>) end;

define method literal-value (x :: <true-literal>) => (value)
  #t
end;
define method literal-value (x :: <false-literal>) => (value)
  #f
end;

define class <null-literal> (<literal>) end;
define method literal-value (x :: <null-literal>) => (value)
  #f
end;

define abstract class <integer-literal> (<literal>)
  constant slot literal-value :: <abstract-integer>,
    required-init-keyword: value:;
end;

define generic integer-literal-radix (lit :: <integer-literal>)
 => (radix :: <integer>);

define abstract class <hex-literal> (<integer-literal>)
  constant class slot integer-literal-radix :: <integer> = 16;
end;

define abstract class <decimal-literal> (<integer-literal>)
  constant class slot integer-literal-radix :: <integer> = 10;
end;

define abstract class <octal-literal> (<integer-literal>)
  constant class slot integer-literal-radix :: <integer> = 8;
end;

define abstract class <int-literal> (<integer-literal>) end;
define abstract class <long-literal> (<integer-literal>) end;

define class <hex-int-literal> (<int-literal>, <hex-literal>) end;
define class <hex-long-literal> (<long-literal>, <hex-literal>) end;
define class <decimal-int-literal> (<int-literal>, <decimal-literal>) end;
define class <decimal-long-literal> (<long-literal>, <decimal-literal>) end;
define class <octal-int-literal> (<int-literal>, <octal-literal>) end;
define class <octal-long-literal> (<long-literal>, <octal-literal>) end;

define abstract class <float-literal> (<literal>)
  constant slot float-literal-mantissa :: <abstract-integer>,
    required-init-keyword: int:;
  constant slot float-literal-scale :: <integer>,
    required-init-keyword: scale:;
  constant slot float-literal-exponent :: <abstract-integer>,
    required-init-keyword: expt:;
end;

define class <single-float-literal> (<float-literal>) end;
define class <double-float-literal> (<float-literal>) end;

define method literal-value (x :: <float-literal>) => (val :: <double-float>)
  // TODO: do better by more direct conversion.
  let exponent = generic--(x.float-literal-exponent, x.float-literal-scale);
  let m :: <double-float> = as(<double-float>, x.float-literal-mantissa);
  let e :: <double-float> = as(<double-float>, 10) ^ exponent;
  generic-*(m, e);
end;

define class <character-literal> (<literal>)
  constant slot literal-value :: <integer>,
    required-init-keyword: value:;
end;

define abstract class <string-literal> (<literal>) end;

define class <byte-string-literal> (<string-literal>)
  constant slot literal-value :: <byte-string>,
    required-init-keyword: value:;
end;

define class <unicode-string-literal> (<string-literal>)
  constant slot literal-value :: <unicode-string>,
    required-init-keyword: value:;
end;


//
define class <identifier> (<token>, <name>)
  constant slot identifier-name :: <byte-string>, required-init-keyword: name:;
end;

define method name-identifiers (id :: <identifier>)
  make(<identifiers>, size: 1, fill: id);
end;

define method token-parse-class (x :: <identifier>) $identifier-token end;

define class <primitive-type> (<token>, <type-descriptor>)
  constant slot primitive-type-symbol :: <symbol>,
    required-init-keyword: name:;
end;

define method type-name (type :: <primitive-type>) => (name :: <primitive-type>)
  type
end;

define method type-numdims (type :: <primitive-type>) => (n :: singleton(0))
  0
end;

define method token-parse-class (x :: <primitive-type>)
  $primitive-type-token
end;

define class <this-token> (<token>, <expression>) end;

define method token-parse-class (x :: <this-token>)
  $%this-token
end;

define class <super-token> (<token>, <expression>) end;

define method token-parse-class (x :: <super-token>)
  $%super-token
end;

// Note that special tokens which are not operators never get seen outside
// of here...
define class <special-token> (<token>)
  constant slot token-parse-class,
    required-init-keyword: class:;
end;

define class <operator> (<special-token>)
  constant slot operator-symbol :: <symbol>, required-init-keyword: name:;
end;

define class <assignment-operator> (<operator>)
  constant slot assignment-operator :: false-or(<operator>),
    required-init-keyword: op:;
end;


define class <lexer-state> (<grammar-object>)
  constant slot lexer-identifiers :: <string-table> = make(<string-table>);
end;

define method make-java-lexer (contents :: <byte-vector>) => (fn :: <function>)
  let ls = make(<lexer-state>);
  let posn :: <integer> = 0;
  local method next-token () => (next-token-class, next-token-value)
          let (token, new-pos) =
            get-token-from-contents($java-tokenizer, contents, posn, ls);
          posn := new-pos;
          if (token == #f)
            values($eof-token, #f)
          else
            values(token.token-parse-class, token)
          end;
        end method;
  next-token
end method make-java-lexer;

define method make-java-lexer (stream :: <stream>) => (fn :: <function>)
  let bytes = stream.stream-size;
  let contents :: <byte-vector> = make(<byte-vector>, size: bytes);
  read-into!(stream, bytes, contents);
  make-java-lexer(contents)
end method make-java-lexer;

define method make-java-lexer (file :: <locator>) => (fn :: <function>)
  let contents :: <byte-vector>
    =  with-open-file (stream = file, element-type: <byte>)
         read-to-end(stream)
       end with-open-file;
  make-java-lexer(contents)
end method make-java-lexer;


define method make-java-lexer (source :: <string>) => (fn :: <function>)
  let contents :: <byte-vector> = as(<byte-vector>, source);
  make-java-lexer(contents);
end method make-java-lexer;


define constant $predefined-tokens :: <string-table> = make(<string-table>);

define inline-only function def-reserved (name, class, #rest keys)
  $predefined-tokens[name] := apply(make, class, name: as(<symbol>, name), keys);
end;

begin
  $predefined-tokens["true"] := make(<true-literal>);
  $predefined-tokens["false"] := make(<false-literal>);
  $predefined-tokens["null"] := make(<null-literal>);


  // reserved but not used
  def-reserved("const", <special-token>, class: -1);
  def-reserved("goto", <special-token>, class: -1);


  def-reserved("abstract", <special-token>, class: $%abstract-token);
  def-reserved("boolean", <primitive-type>);
  def-reserved("break", <special-token>, class: $%break-token);
  def-reserved("byte", <primitive-type>);
  def-reserved("case", <special-token>, class: $%case-token);
  def-reserved("catch", <special-token>, class: $%catch-token);
  def-reserved("char", <primitive-type>);
  def-reserved("class", <special-token>, class: $%class-token);
  def-reserved("continue", <special-token>, class: $%continue-token);
  def-reserved("default", <special-token>, class: $%default-token);
  def-reserved("do", <special-token>, class: $%do-token);
  def-reserved("double", <primitive-type>);
  def-reserved("else", <special-token>, class: $%else-token);
  def-reserved("extends", <special-token>, class: $%extends-token);
  def-reserved("final", <special-token>, class: $%final-token);
  def-reserved("finally", <special-token>, class: $%finally-token);
  def-reserved("float", <primitive-type>);
  def-reserved("for", <special-token>, class: $%for-token);
  def-reserved("if", <special-token>, class: $%if-token);
  def-reserved("implements", <special-token>, class: $%implements-token);
  def-reserved("import", <special-token>, class: $%import-token);
  def-reserved("instanceof", <operator>, class: $%instanceof-token);
  def-reserved("int", <primitive-type>);
  def-reserved("interface", <special-token>, class: $%interface-token);
  def-reserved("long", <primitive-type>);
  def-reserved("native", <special-token>, class: $%native-token);
  def-reserved("new", <special-token>, class: $%new-token);
  def-reserved("package", <special-token>, class: $%package-token);
  def-reserved("private", <special-token>, class: $%private-token);
  def-reserved("protected", <special-token>, class: $%protected-token);
  def-reserved("public", <special-token>, class: $%public-token);
  def-reserved("return", <special-token>, class: $%return-token);
  def-reserved("short", <primitive-type>);
  def-reserved("static", <special-token>, class: $%static-token);
  def-reserved("super", <super-token>);
  def-reserved("switch", <special-token>, class: $%switch-token);
  def-reserved("synchronized", <special-token>, class: $%synchronized-token);
  def-reserved("this", <this-token>);
  def-reserved("throw", <special-token>, class: $%throw-token);
  def-reserved("throws", <special-token>, class: $%throws-token);
  def-reserved("transient", <special-token>, class: $%transient-token);
  def-reserved("try", <special-token>, class: $%try-token);
  def-reserved("void", <special-token>, class: $%void-token);
  def-reserved("volatile", <special-token>, class: $%volatile-token);
  def-reserved("while", <special-token>, class: $%while-token);

  def-reserved("(", <special-token>, class: $%lparen-token);
  def-reserved(")", <special-token>, class: $%rparen-token);
  def-reserved("{", <special-token>, class: $%lbrace-token);
  def-reserved("}", <special-token>, class: $%rbrace-token);
  def-reserved("[", <special-token>, class: $%lbracket-token);
  def-reserved("]", <special-token>, class: $%rbracket-token);
  def-reserved(";", <special-token>, class: $%semi-colon-token);
  def-reserved(",", <special-token>, class: $%comma-token);
  def-reserved(".", <special-token>, class: $%dot-token);
  def-reserved("?", <special-token>, class: $%qmark-token);
  def-reserved(":", <special-token>, class: $%colon-token);

  def-reserved("=", <assignment-operator>, class: $%=-token, op: #f);
  def-reserved(">", <operator>, class: $%>-token);
  def-reserved("<", <operator>, class: $%<-token);
  def-reserved("!", <operator>, class: $%!-token);
  def-reserved("~", <operator>, class: $%~-token);
  def-reserved("==", <operator>, class: $%==-token);
  def-reserved(">=", <operator>, class: $%>=-token);
  def-reserved("<=", <operator>, class: $%<=-token);
  def-reserved("!=", <operator>, class: $%!=-token);
  def-reserved("&&", <operator>, class: $%&&-token);
  def-reserved("||", <operator>, class: $%||-token);
  def-reserved("+", <operator>, class: $%+-token);
  def-reserved("-", <operator>, class: $%--token);
  def-reserved("*", <operator>, class: $%*-token);
  def-reserved("/", <operator>, class: $%/-token);
  def-reserved("&", <operator>, class: $%&-token);
  def-reserved("|", <operator>, class: $%|-token);
  def-reserved("^", <operator>, class: $%^-token);
  def-reserved("%", <operator>, class: $%%-token);
  def-reserved(">>", <operator>, class: $%>>-token);
  def-reserved("<<", <operator>, class: $%<<-token);
  def-reserved(">>>", <operator>, class: $%>>>-token);
  def-reserved("++", <assignment-operator>, class: $%++-token,
               op: $predefined-tokens["+"]);
  def-reserved("--", <assignment-operator>, class: $%---token,
               op: $predefined-tokens["-"]);
  def-reserved("+=", <assignment-operator>, class: $%+=-token,
               op: $predefined-tokens["+"]);
  def-reserved("-=", <assignment-operator>, class: $%-=-token,
               op: $predefined-tokens["-"]);
  def-reserved("*=", <assignment-operator>, class: $%*=-token,
               op: $predefined-tokens["*"]);
  def-reserved("/=", <assignment-operator>, class: $%/=-token,
               op: $predefined-tokens["/"]);
  def-reserved("&=", <assignment-operator>, class: $%&=-token,
               op: $predefined-tokens["&"]);
  def-reserved("|=", <assignment-operator>, class: $%|=-token,
               op: $predefined-tokens["|"]);
  def-reserved("^=", <assignment-operator>, class: $%^=-token,
               op: $predefined-tokens["^"]);
  def-reserved("%=", <assignment-operator>, class: $%%=-token,
               op: $predefined-tokens["%"]);
  def-reserved("<<=", <assignment-operator>, class: $%<<=-token,
               op: $predefined-tokens["<<"]);
  def-reserved(">>=", <assignment-operator>, class: $%>>=-token,
               op: $predefined-tokens[">>"]);
  def-reserved(">>>=", <assignment-operator>, class: $%>>>=-token,
               op: $predefined-tokens[">>>"]);
end;

define function make-identifier (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <token>)
  element($predefined-tokens, string, default: #f)
    | element(ls.lexer-identifiers, string, default: #f)
    | (ls.lexer-identifiers[string] := make(<identifier>, name: string))
end;

define function parse-decimal-literal (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <integer-literal>)
  let length = string.size;
  iterate loop (posn :: <integer> = 0, res :: <abstract-integer> = 0)
    if (posn < length)
      let digit :: <integer> = as(<integer>, string[posn]);
      if (digit == as(<integer>, 'l') | digit == as(<integer>, 'L'))
        make(<decimal-long-literal>, value: res)
      else
        loop(posn + 1, generic-+(generic-*(res, 10), digit - 48))
      end
    else
      make(<decimal-int-literal>, value: res)
    end;
  end iterate;
end;

define function parse-octal-literal (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <integer-literal>)
  let length = string.size;
  iterate loop (posn :: <integer> = 0, res :: <abstract-integer> = 0)
    if (posn < length)
      let digit :: <integer> = as(<integer>, string[posn]);
      if (digit == as(<integer>, 'l') | digit == as(<integer>, 'L'))
        make(<octal-long-literal>, value: res)
      else
        loop(posn + 1, generic-+(generic-*(res, 8), digit - 48))
      end
    else
      make(<octal-int-literal>, value: res)
    end;
  end iterate;
end;

define function parse-hex-literal (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <integer-literal>)
  let length = string.size;
  iterate loop (posn :: <integer> = 2, res :: <abstract-integer> = 0)
    if (posn < length)
      let digit :: <integer> = logior(32, as(<integer>, string[posn]));
      if (digit == as(<integer>, 'l'))
        make(<hex-long-literal>, value: res)
      else
        let value = if (digit >= 97) digit - 87 else digit - 48 end;
        loop(posn + 1, generic-+(generic-*(res, 16), value))
      end
    else
      make(<hex-int-literal>, value: res)
    end;
  end iterate;
end;

define function parse-float-literal (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <float-literal>)
  let length = string.size;
  local method edigits (m :: <abstract-integer>, s :: <integer>,
                        sign :: one-of(1, -1),
                        posn :: <integer>, res :: <abstract-integer>)
          if (posn < length)
            let digit = as(<integer>, string[posn]);
            if (digit == as(<integer>, 'f') | digit == as(<integer>, 'F'))
              make(<single-float-literal>, mantissa: m, scale: s,
                   expt: generic-*(res, sign));
            elseif (digit == as(<integer>, 'D') | digit == as(<integer>, 'D'))
              make(<double-float-literal>, mantissa: m, scale: s,
                   expt: generic-*(res, sign));
            else
              edigits(m, s, sign, posn + 1, generic-+(generic-*(res, 10), digit))
            end
          else
            make(<double-float-literal>, mantissa: m, scale: s,
                 expt: generic-*(res, sign));
          end;
        end method;
  local method fexpt (m :: <abstract-integer>, s :: <integer>, posn :: <integer>)
          let sign = string[posn];
          if (sign == '+')
            edigits(m, s, 1, posn + 1, 0)
          elseif (sign == '-')
            edigits(m, s, -1, posn + 1, 0)
          else
            edigits(m, s, 1, posn, 0)
          end;
        end method;
  local method fdigits (posn :: <integer>,
                        m :: <abstract-integer>, s :: <integer>)
          if (posn < length)
            let digit :: <integer> = as(<integer>, string[posn]);
            if (digit == as(<integer>, 'e') | digit == as(<integer>, 'E'))
              fexpt(m, s, posn + 1);
            elseif (digit == as(<integer>, 'f') | digit == as(<integer>, 'F'))
              make(<single-float-literal>, mantissa: m, scale: s, expt: 0);
            elseif (digit == as(<integer>, 'D') | digit == as(<integer>, 'D'))
              make(<double-float-literal>, mantissa: m, scale: s, expt: 0);
            else
              fdigits(posn + 1, generic-+(generic-*(m, 10), digit), s + 1);
            end
          else
            make(<double-float-literal>, mantissa: m, scale: s, expt: 0);
          end;
        end method;
  iterate idigits (posn :: <integer> = 0, res :: <abstract-integer> = 0)
    let digit :: <integer> = as(<integer>, string[posn]);
    if (digit == as(<integer>, '.'))
      fdigits(posn + 1, res, 0)
    elseif (digit == as(<integer>, 'e') | digit == as(<integer>, 'E'))
      fexpt(res, 0, posn + 1);
    elseif (digit == as(<integer>, 'f') | digit == as(<integer>, 'F'))
      make(<single-float-literal>, mantissa: res, scale: 0, expt: 0);
    elseif (digit == as(<integer>, 'D') | digit == as(<integer>, 'D'))
      make(<double-float-literal>, mantissa: res, scale: 0, expt: 0);
    else
      idigits(posn + 1, generic-+(generic-*(res, 10), digit))
    end
  end iterate;

end;

define function parse-escape (string, posn)
  let ch = string[posn];
  let nposn = posn + 1;
  select (ch)
    'b' => values(8, nposn);
    't' => values(as(<integer>, '\t'), nposn);
    'n' => values(as(<integer>, '\n'), nposn);
    'f' => values(as(<integer>, '\f'), nposn);
    'r' => values(as(<integer>, '\r'), nposn);
    '"' => values(as(<integer>, '"'), nposn);
    '\'' => values(as(<integer>, '\''), nposn);
    '\\' => values(as(<integer>, '\\'), nposn);
    otherwise => iterate loop (nposn :: <integer> = posn, res :: <integer> = 0)
                   let digit = as(<integer>, string[nposn]);
                   if (nposn < posn + 3 & 48 <= digit & digit <= 55)
                     loop(nposn + 1, res * 8 + digit - 48)
                   else
                     values(res, nposn)
                   end
                 end iterate;
  end;
end;


define function parse-character-literal (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <character-literal>)
  let ch = string[1];
  let n = if (ch ~== '\\') as(<integer>, ch) else parse-escape(string, 2) end;
  make(<character-literal>, value: n);
end;

define function parse-string-literal (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <string-literal>)
  iterate loop (inpos :: <integer> = 1, opos :: <integer> = 0)
    let ch = string[inpos];
    if (ch == '\"')
      make(<byte-string-literal>, value: copy-sequence(string, end: opos));
    elseif (ch ~== '\\')
      string[opos] := ch;
      loop(inpos + 1, opos + 1);
    else
      let (nch, npos) = parse-escape(string, inpos + 1);
      if (nch >= 255) // oops
        parse-unicode-string-literal(ls, string)
      else
        string[opos] := as(<character>, nch);
        loop(npos, opos + 1);
      end;
    end;
  end iterate;
end;

define function parse-unicode-string-literal (ls :: <lexer-state>, string :: <byte-string>)
 => (token :: <string-literal>)
  let v :: <unicode-string> = make(<unicode-string>,
                                   size: string.size - 2,
                                   fill: as(<unicode-character>, 0));
  iterate loop (inpos :: <integer> = 1, opos :: <integer> = 0)
    let ch = string[inpos];
    if (ch == '\"')
      make(<unicode-string-literal>,
           value: if (opos == v.size) v else copy-sequence(v, end: opos) end);
    elseif (ch ~== '\\')
      v[opos] := as(<unicode-character>, ch);
      loop(inpos + 1, opos + 1);
    else
      let (nch, npos) = parse-escape(string, inpos + 1);
      v[opos] := as(<unicode-character>, nch);
      loop(npos, opos + 1);
    end;
  end iterate;
end;

define state-machine $java-tokenizer
  state start: #f,
    start: " \t\f\r\n\<0B>\<1A>", // vertical tab, 1A = ^Z
    slash: '/',
    symbol: "A-Za-z_$",
    zero: '0',
    decimal-digits: "1-9",
    dot: '.',
    char-quote: '\'',
    string-quote: '\"',
    operator: "(){}[];,~?:",
    operator-pre-equal: "=!*^%", // ! !=
    and: '&', // & && &=
    or: '|',
    plus: '+',
    minus: '-',
    greater: '>', // > >= >> >>> >>= >>>=
    lesser: '<';  // < <= << <<=

  state slash: make-identifier,
    single-line-comment: '/',
    multi-line-comment: '*',
    slash-equal: '=';
  state single-line-comment: #f,
    start: "\r\n",
    single-line-comment: "\<00>-\<09>\<0B>\<0C>\<0E>-\<FF>";
  state multi-line-comment: #f,
    multi-line-comment-star: '*',
    multi-line-comment: "\<00>-)+-\<FF>";
  state multi-line-comment-star: #f,
    start: '/',
    multi-line-comment-star: '*',
    multi-line-comment: "\<00>-)+-.0-\<FF>";

  state slash-equal: make-identifier;

  state symbol: make-identifier,
    symbol: "A-Za-z_$0-9";

  state dot: make-identifier,
    float-after-dot: "0-9";

  state zero: parse-decimal-literal,
    decimal-parsed: "Ll",
    hex: "xX",
    octal: "0-7",
    decimal-digits: "89",
    float-after-dot: ".",
    float-after-expt: "Ee",
    float-parsed: "fdFD";
  state decimal-digits: parse-decimal-literal,
    decimal-parsed: "Ll",
    decimal-digits: "0-9",
    float-after-dot: ".",
    float-after-expt: "Ee",
    float-parsed: "fdFD";
  state decimal-parsed: parse-decimal-literal;
  state octal: parse-octal-literal,
    octal-parsed: "Ll",
    octal: "0-7",
    float-digits: "89", // Can't be decimal, because started with 0.
    float-after-dot: ".",
    float-after-expt: "Ee",
    float-parsed: "fdFD";
  state octal-parsed: parse-octal-literal;
  state hex: parse-hex-literal,
    hex-parsed: "Ll",
    hex: "0-9A-Fa-f";
  state hex-parsed: parse-hex-literal;
  state float-digits: parse-float-literal,
    float-digits: "0-9",
    float-after-dot: ".",
    float-after-expt: "Ee",
    float-parsed: "fdFD";
  state float-after-dot: parse-float-literal,
    float-after-dot: "0-9",
    float-after-expt: "Ee",
    float-parsed: "fdFD";
  state float-after-expt: parse-float-literal,
    float-in-expt: "0-9+",
    float-in-expt: '-',
    float-parsed: "fdFD";
  state float-in-expt: parse-float-literal,
    float-in-expt: "0-9",
    float-parsed: "fdFD";
  state float-parsed: parse-float-literal;

  state char-quote: #f,
    close-char-quote: "\<00>-\<09>\<0B>\<0C>\<0E>-&(-[]-\<FF>",
    char-escape: '\\';
  state char-escape: #f,
    close-char-quote: "\\'\"btnfr",
    octal-char-quote-2: "0-3",
    octal-char-quote-1: "4-7";
  state octal-char-quote-2: #f,
    octal-char-quote-1: "0-7",
    character: '\'';
  state octal-char-quote-1: #f,
    close-char-quote: "0-7",
    character: '\'';
  state close-char-quote: #f,
    character: '\'';
  state character: parse-character-literal;

  state string-quote: #f,
    string: '"',
    string-escape: '\\',
    string-quote: "\<00>-\<09>\<0B>\<0C>\<0E>-!#-[]-\<FF>";
  state string-escape: #f,
    string-quote: "\\'\"btnfr0-7";
  state string: parse-string-literal;

  state and: make-identifier, // & && &=
    operator: '&',
    operator: '=';

  state or: make-identifier, // | || |=
    operator: '|',
    operator: '=';

  state plus: make-identifier, // + ++ +=
    operator: '+',
    operator: '=';

  state minus: make-identifier, // - -- -=
    operator: '-',
    operator: '=';

  state greater: make-identifier,  // > >= >> >>> >>= >>>=
    operator: '=',
    greater-greater: '>';
  state greater-greater: make-identifier, // >> >>> >>= >>>=
    operator: '=',
    operator-pre-equal: '>';

  state lesser: make-identifier,   // < <= << <<=
    operator: '=',
    operator-pre-equal: '<';

  state operator-pre-equal: make-identifier,
    operator: '=';

  state operator: make-identifier;
end state-machine;

