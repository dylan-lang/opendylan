Module:   lexer
Synopsis: We don't have a lexer generator at the momement so we'll just
          have to pretend.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Lexer state and methods:

define class <lexer-state> (<object>)
  slot buf :: <byte-string>,
    init-function: method () make(<byte-string>, size: 256) end;
  slot buf-ptr :: <integer>,
    init-value: 0;
  slot stream,
    required-init-keyword: stream:;
  slot unread,
    init-value: #f;
  slot classifier,
    init-keyword: classifier:,
    init-value:   default-classifier;
end class;

define method buffer! (s :: <lexer-state>, c :: <character>)
  let the-buf :: <byte-string> = s.buf;
  let the-ptr :: <integer> = s.buf-ptr;
  the-buf[the-ptr] := c;
  let the-next-ptr :: <integer> = the-ptr + 1;
  s.buf-ptr := the-next-ptr;
end method;

define method reset-buffer! (s :: <lexer-state>)
  s.buf-ptr := 0;
end method;

define constant $eof-marker = as(<character>, -1);

define method lex-next (s :: <lexer-state>)
  let cached = s.unread;
  if (cached)
    s.unread := #f;
    cached
  else
    read-element(s.stream, on-end-of-stream: $eof-marker)
  end
end method;

define method unlex-next (s :: <lexer-state>, c :: <character>)
  s.unread := c;
end method;

define method lex-next-buffering (s :: <lexer-state>)
  let next :: <character> = lex-next(s);
  buffer!(s, next);
  next
end method;

define method lex-while (s :: <lexer-state>, ok? :: <function>)
  for (next :: <character> = lex-next(s) then lex-next(s),
       while: ok?(next))
    buffer!(s, next);
  finally
    unlex-next(s, next);
  end;
end method;

// eof
