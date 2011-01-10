Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define table $tablegen-keywords :: <string-table>
  = {
     "int"        => $%INT-token,
     "bit"        => $%BIT-token,
     "bits"       => $%BITS-token,
     "string"     => $%STRING-token,
     "list"       => $%LIST-token,
     "code"       => $%CODE-token,
     "dag"        => $%DAG-token,
     "class"      => $%CLASS-token,
     "def"        => $%DEF-token,
     "defm"       => $%DEFM-token,
     "multiclass" => $%MULTICLASS-token,
     "field"      => $%FIELD-token,
     "let"        => $%LET-token,
     "in"         => $%IN-token,
     "include"    => #"include",
    };

define table $tablegen-!keywords :: <string-table>
  = {
     "con"        => $%CONCATTOK-token,
     "sra"        => $%SRATOK-token,
     "srl"        => $%SRLTOK-token,
     "shl"        => $%SHLTOK-token,
     "eq"         => $%EQTOK-token,
     "strconcat"  => $%STRCONCATTOK-token,
     "nameconcat" => $%NAMECONCATTOK-token,
     "if"         => $%IFTOK-token,
     "subst"      => $%SUBSTTOK-token,
     "foreach"    => $%FOREACHTOK-token,
     "cast"       => $%CASTTOK-token,
     "car"        => $%CARTOK-token,
     "cdr"        => $%CDRTOK-token,
     "null"       => $%NULLTOK-token,
    };

define function tablegen-identifier-character?
    (ch :: <character>) => (result? :: <boolean>)
  ('a' <= ch & ch <= 'z')
  | ('A' <= ch & ch <= 'Z')
  | ('0' <= ch & ch <= '9')
  | ch == '_'
  | ch == '#'
end function;


define function tablegen-parse
    (stream :: <stream>, search-path :: <sequence>)
 => ();
  let stream-stack = make(<object-deque>);
  local
    method lexer () => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      select (ch)
        #f =>
          values($EOF-token, #f);

        ' ', '\t', '\r', '\n' =>
          lexer();

        ':' =>
          values($%COLON-token, #f);
        ';' =>
          values($%SEMI-token, #f);
        '.' =>
          values($%DOT-token, #f);
        ',' =>
          values($%COMMA-token, #f);
        '<' =>
          values($%LANGLE-token, #f);
        '>' =>
          values($%RANGLE-token, #f);
        '[' =>
          lexer-lbrack();
        ']' =>
          values($%RBRACK-token, #f);
        '{' =>
          values($%LBRACE-token, #f);
        '}' =>
          values($%RBRACE-token, #f);
        '(' =>
          values($%LPAREN-token, #f);
        ')' =>
          values($%RPAREN-token, #f);
        '=' =>
          values($%EQ-token, #f);
        '?' =>
          values($%QUEST-token, #f);

        '/' =>
          lexer-/();

        '-', '+' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-integer(characters, 10);

        '0' =>
          lexer-0();

        '1', '2', '3', '4', '5', '6', '7', '8', '9' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-integer(characters, 10);

        '"' =>
          let characters = make(<stretchy-object-vector>);
          lexer-quote(characters);

        '$' =>
          let characters = make(<stretchy-object-vector>);
          lexer-varname(characters);

        '!' =>
          let characters = make(<stretchy-object-vector>);
          lexer-!(characters);

        otherwise =>
          if (tablegen-identifier-character?(ch))
            let characters = make(<stretchy-object-vector>);
            add!(characters, ch);
            lexer-identifier(characters);
          else
            error("Unrecognized character '%c'", ch);
          end if;
      end select;
    end method,

    method lexer-lbrack () => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch = '{')
        error("lexer-lbrack codefragment");
      else
        values($%LBRACK-token, #f);
      end if
    end method,

    method lexer-/ () => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if (ch == '/')
        lexer-bcpl-comment();
      elseif (ch == '*')
        lexer-c-comment(1);
      else
        error("Unrecognized '/%c'", ch);
      end if;
    end method,

    method lexer-bcpl-comment () => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if (~ch)
        values($EOF-token, #f);
      elseif (ch == '\r' | ch == '\n')
        lexer();
      else
        lexer-bcpl-comment();
      end if;
    end method,

    method lexer-c-comment (level :: <integer>) => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if (~ch)
        error("end-of-file within /* comment */");
      elseif (ch == '*')
        lexer-c-comment-*(level);
      elseif (ch == '/')
        lexer-c-comment-/(level)
      else
        lexer-c-comment(level)
      end if;
    end method,

    method lexer-c-comment-*
        (level :: <integer>) => (token-class, token-value);
      error("lexer-c-comment-*");
    end method,

    method lexer-c-comment-/
        (level :: <integer>) => (token-class, token-value);
      error("lexer-c-comment-/");
    end method,

    method lexer-0
        ()  => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == 'x')
        read-element(stream);
        let characters = make(<stretchy-object-vector>);
        lexer-integer(characters, 16)
      elseif (ch == 'b')
        read-element(stream);
        let characters = make(<stretchy-object-vector>);
        lexer-integer(characters, 2)
      elseif ('0' <= ch & ch <= '7')
        let characters = make(<stretchy-object-vector>);
        add!(characters, read-element(stream));
        lexer-integer(characters, 8)
      else
        values($%INTVAL-token, 0)
      end if
    end method,

    method lexer-integer
        (characters :: <stretchy-object-vector>, base :: <integer>)
     => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-integer(characters, base);
      elseif (base = 16 & (('A' <= ch & ch <= 'F') | ('a' <= ch & ch <= 'f')))
        add!(characters, read-element(stream));
        lexer-integer(characters, base);
      else
        let number = as(<string>, characters);
        if (number = "-")
          values($%MINUS-token, #f)
        else
          values($%INTVAL-token, string-to-integer(number, base: base))
        end if
      end if;
    end method,

    method lexer-quote
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if (~ch)
        error("End of file within quoted string");
      elseif (ch == '"')
        values($%STRVAL-token, as(<string>, characters))
      else
        add!(characters, ch);

        lexer-quote(characters)
      end if;
    end method,

    method lexer-identifier
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (tablegen-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      else
        let identifier = as(<string>, characters);
        let token = element($tablegen-keywords, identifier, default: #f);
        if (token)
          values(token, identifier)
        else
          values($%ID-token, identifier)
        end if
      end if
    end method,

    method lexer-varname
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (tablegen-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-varname(characters);
      else
        let identifier = as(<string>, characters);
        values($%VARNAME-token, identifier)
      end if
    end method,

    method lexer-!
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (tablegen-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-!(characters);
      else
        let identifier = as(<string>, characters);
        let token = element($tablegen-!keywords, identifier, default: #f);
        if (token)
          values(token, identifier)
        else
          error("Unrecognized operator !%s", identifier);
        end if
      end if
    end method,

    method screener () => (token-class, token-value);
      let (token-class, token-value) = lexer();
      if (token-class == #"include")
        let (string-token-class, string-token-value) = lexer();
        if (string-token-class == $%STRVAL-token)
          push(stream-stack, stream);
          stream := open-include-file(string-token-value, search-path);
          screener();
        else
          error("include not followed by string");
        end if
      elseif (token-class == $EOF-token & ~empty?(stream-stack))
        close(stream);
        stream := pop(stream-stack);
        screener();
      else
        values(token-class, token-value)
      end if
    end method,

    method on-error (symbol, value, history) => ()
      error("syntax error at %= (%=)", symbol, value);
    end method;

  run-parser(#f, tablegen-parser, screener, on-error: on-error);
end function;

define function open-include-file
    (filename :: <string>, include-path :: <sequence>)
 => (stream :: <stream>);
  let locator = as(<file-locator>, filename);
  try-open-include-file(locator)
    | block (return)
        for (directory in include-path)
          let merged = merge-locators(locator, directory);
          let stream = try-open-include-file(merged);
          if (stream)
            return(stream);
          end if;
        end for;
        error("include file %s not found in include path", filename);
      end block
end function;

define function try-open-include-file
    (locator :: <file-locator>)
 => (stream :: false-or(<stream>));
  block (return)
    open-file-stream(locator, direction: #"input")
  exception (e :: <file-does-not-exist-error>)
    return(#f);
  end block
end function;
