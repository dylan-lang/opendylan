Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <jam-input-mode>
  = one-of(#"normal", #"string", #"punctuation");

define class <jam-input-state> (<object>)
  slot jam-input-mode :: <jam-input-mode>,
    init-value: #"normal";
  constant slot jam-input-data :: <byte-string>,
    init-keyword: input-data:;
  slot jam-input-position :: <integer>,
    init-value: 0, init-keyword: input-position:;
  slot jam-input-line-number :: <integer>,
    init-value: 1, init-keyword: input-line-number:;
  constant slot jam-input-locator :: false-or(<locator>),
    init-keyword: input-locator:;
end class;

define thread variable *jam-input-state* :: <jam-input-state>
  = make(<jam-input-state>, input-data: "");

define variable *jam-toplevel-statements* :: <sequence> = #[];

define function jam-read-file
    (jam :: <jam-state>, jamfile :: <locator>)
 => ();
  let input-data :: <byte-string>
    = with-open-file(stream = jamfile)
        read-to-end(stream)
      end;
  jam-read(jam, input-data, jamfile)
end;

define function jam-read
    (jam :: <jam-state>,
     input-data :: <byte-string>,
     jamfile :: false-or(<locator>))
 => ();
  local
    method lexer() => (token-class, token-value);
      let data = *jam-input-state*.jam-input-data;
      let _end :: <integer> = data.size;
      
      if (*jam-input-state*.jam-input-mode == #"string")
        iterate loop (pos :: <integer>
                        = *jam-input-state*.jam-input-position,
                      level :: <integer> = 1)
          if (pos = _end)
            error("action string at end-of-file in %s",
                  *jam-input-state*.jam-input-locator);
          else
            select (data[pos])
              '{' =>
                loop(pos + 1, level + 1);
                
              '}' =>
                if (level = 1)
                  let string
                    = copy-sequence
                    (data, start: *jam-input-state*.jam-input-position,
                     end: pos);
                  *jam-input-state*.jam-input-position := pos;
                  values($%STRING-token, string)
                else
                  loop(pos + 1, level - 1)
                end;
                
              '\n' =>
                *jam-input-state*.jam-input-line-number
                  := *jam-input-state*.jam-input-line-number + 1;
                loop(pos + 1, level);
                
              '\r' =>
                unless (pos + 1 < _end & data[pos + 1] = '\n')
                  *jam-input-state*.jam-input-line-number
                    := *jam-input-state*.jam-input-line-number + 1;
                end unless;
                loop(pos + 1, level);
                
              otherwise =>
                loop(pos + 1, level);
            end select;
          end if;
        end iterate;
      else
        iterate loop (start :: <integer>
                        = *jam-input-state*.jam-input-position,
                      pos :: <integer>
                        = *jam-input-state*.jam-input-position,
                      metachars? :: <boolean> = #f,
                      quoting? :: <boolean> = #f,
                      accumulated-string :: <byte-string> = "")
          if (start = _end)
            values($EOF-token, #f)
          elseif (pos = _end)
            if (quoting?)
              error("unterminated quoted string at end-of-file in %s",
                    *jam-input-state*.jam-input-locator);
            end if;
            let str
              = concatenate(accumulated-string,
                            copy-sequence(data, start: start, end: pos));
            *jam-input-state*.jam-input-position := pos;
            return-token(str, metachars?)
          elseif (quoting?)
            let char = data[pos];
            select (char by \=)
              '"' =>
                if (pos > start)
                  let str
                    = concatenate(accumulated-string,
                                  copy-sequence(data,
                                                start: start, end: pos));
                  loop(pos + 1, pos + 1, #t, #f, str);
                else
                  loop(pos + 1, pos + 1, #t, #f, accumulated-string);
                end if;
                
              '\\' =>
                if (pos > start)
                  let str
                    = concatenate(accumulated-string,
                                  copy-sequence(data,
                                                start: start, end: pos));
                  loop(pos + 1, pos + 2, #t, #t, str);
                else
                  loop(pos + 1, pos + 2, #t, #t, accumulated-string);
                end if;
                
              '\n' =>
                *jam-input-state*.jam-input-line-number
                  := *jam-input-state*.jam-input-line-number + 1;
                loop(start, pos + 1, metachars?, quoting?,
                     accumulated-string);
                
              '\r' =>
                unless (pos + 1 < _end & data[pos + 1] = '\n')
                  *jam-input-state*.jam-input-line-number
                    := *jam-input-state*.jam-input-line-number + 1;
                end unless;
                loop(start, pos + 1, metachars?, quoting?,
                     accumulated-string);
                
              otherwise =>
                loop(start, pos + 1, metachars?, quoting?,
                     accumulated-string);
                
            end select;
          else
            let char = data[pos];
            select (char by \=)
              ' ', '\t', '\f'  =>
                if (pos > start | metachars?)
                  *jam-input-state*.jam-input-position := pos + 1;
                  let str
                    = concatenate(accumulated-string,
                                  copy-sequence(data,
                                                start: start, end: pos));
                  return-token(str, metachars?)
                else
                  loop(pos + 1, pos + 1, metachars?, quoting?,
                       accumulated-string)
                end;
                
              '\n' =>
                *jam-input-state*.jam-input-line-number
                  := *jam-input-state*.jam-input-line-number + 1;
                if (pos > start | metachars?)
                  *jam-input-state*.jam-input-position := pos + 1;
                  let str
                    = concatenate(accumulated-string,
                                  copy-sequence(data,
                                                start: start, end: pos));
                  return-token(str, metachars?)
                else
                  loop(pos + 1, pos + 1, metachars?, quoting?,
                       accumulated-string)
                end;
                
              '\r' =>
                unless (pos + 1 < _end & data[pos + 1] = '\n')
                  *jam-input-state*.jam-input-line-number
                    := *jam-input-state*.jam-input-line-number + 1;
                end unless;
                if (pos > start | metachars?)
                  *jam-input-state*.jam-input-position := pos + 1;
                  let str
                    = concatenate(accumulated-string,
                                  copy-sequence(data,
                                                start: start, end: pos));
                  return-token(str, metachars?)
                else
                  loop(pos + 1, pos + 1, metachars?, quoting?,
                       accumulated-string)
                end;
                
              '#' =>
                if (pos > start | metachars?)
                  loop(start, pos + 1, #t, quoting?, accumulated-string)
                else
                  for (pos from start below _end,
                       until: data[pos] = '\r' | data[pos] = '\n')
                  finally
                    loop(pos, pos, metachars?, quoting?,
                         accumulated-string)
                  end for
                end;
                
              '"' =>
                if (pos > start)
                  let str
                    = concatenate(accumulated-string,
                                  copy-sequence(data,
                                                start: start, end: pos));
                  loop(pos + 1, pos + 1, #t, #t, str);
                else
                  loop(pos + 1, pos + 1, #t, #t, accumulated-string);
                end if;
                
              '\\' =>
                if (pos > start)
                  let str
                    = concatenate(accumulated-string,
                                  copy-sequence(data,
                                                start: start, end: pos));
                  loop(pos + 1, pos + 2, #t, quoting?, str);
                else
                  loop(pos + 1, pos + 2, #t, quoting?, accumulated-string);
                end if;
                
              otherwise =>
                loop(start, pos + 1, metachars?, quoting?,
                     accumulated-string);
            end select;
          end if;
        end iterate;
      end if;
    end method,

    method return-token
        (token-string :: <byte-string>, metachars? :: <boolean>)
     => (token-class, token-value);
      if (metachars?)
        values($%ARG-token, token-string)
      else
        let token = element($jam-tokens, token-string, default: #f);
        if (token)
          if (token >= $PUNCTUATION)
            values(token - $PUNCTUATION, #f)
          elseif (*jam-input-state*.jam-input-mode ~= #"punctuation")
            values(token, #f);
          else
            values($%ARG-token, token-string)
          end if
        else
          values($%ARG-token, token-string)
        end if
      end if;
    end method,

    method on-error (symbol, value, history) => ()
      if (*jam-input-state*)
        error("%s:%d: syntax error",
              *jam-input-state*.jam-input-locator,
              *jam-input-state*.jam-input-line-number);
      else
        error("syntax error");
      end if;
    end;

  dynamic-bind(*jam-input-state* = make(<jam-input-state>,
                                        input-data: input-data,
                                        input-locator: jamfile),
               *jam-toplevel-statements* = #[])
    run-parser(#f, jam-parser, lexer, on-error: on-error);
    do(curry(evaluate-statement, jam), *jam-toplevel-statements*)
  end
end function;

define constant $PUNCTUATION = 1000;

define table $jam-tokens :: <string-table>
  = {
     "!" => $%BANG-token + $PUNCTUATION,
     "!=" => $%BANG-EQUALS-token + $PUNCTUATION,
     "&" => $%AMPER-token + $PUNCTUATION,
     "&&" => $%AMPERAMPER-token + $PUNCTUATION,
     "(" => $%LPAREN-token + $PUNCTUATION,
     ")" => $%RPAREN-token + $PUNCTUATION,
     "+=" => $%PLUS-EQUALS-token + $PUNCTUATION,
     ":" => $%COLON-token + $PUNCTUATION,
     ";" => $%SEMIC-token + $PUNCTUATION,
     "<" => $%LANGLE-token + $PUNCTUATION,
     "<=" => $%LANGLE-EQUALS-token + $PUNCTUATION,
     "=" => $%EQUALS-token + $PUNCTUATION,
     ">" => $%RANGLE-token + $PUNCTUATION,
     ">=" => $%RANGLE-EQUALS-token + $PUNCTUATION,
     "?=" => $%QUESTION-EQUALS-token + $PUNCTUATION,
     "[" => $%LBRACKET-token + $PUNCTUATION,
     "]" => $%RBRACKET-token + $PUNCTUATION,
     "actions" => $%ACTIONS-token,
     "bind" => $%BIND-token,
     "break" => $%BREAK-token,
     "case" => $%CASE-token,
     "continue" => $%CONTINUE-token,
     "default" => $%DEFAULT-token,
     "else" => $%ELSE-token,
     "existing" => $%EXISTING-token,
     "for" => $%FOR-token,
     "if" => $%IF-token,
     "ignore" => $%IGNORE-token,
     "in" => $%IN-token,
     "include" => $%INCLUDE-token,
     "local" => $%LOCAL-token,
     "maxline" => $%MAXLINE-token,
     "on" => $%ON-token,
     "piecemeal" => $%PIECEMEAL-token,
     "quietly" => $%QUIETLY-token,
     "return" => $%RETURN-token,
     "rule" => $%RULE-token,
     "switch" => $%SWITCH-token,
     "together" => $%TOGETHER-token,
     "updated" => $%UPDATED-token,
     "while" => $%WHILE-token,
     "{" => $%LBRACE-token + $PUNCTUATION,
     "|" => $%BAR-token + $PUNCTUATION,
     "||" => $%BARBAR-token + $PUNCTUATION,
     "}" => $%RBRACE-token + $PUNCTUATION
    };

