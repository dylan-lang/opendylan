Module: lexer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro character-class-definer
  { define character-class ?name = ?chars }
    => { define constant ?name :: <string> 
           = concatenate(?chars, as-uppercase(?chars));
         define constant ?name ## \-table
           = make-boolean-table(?name);
         define constant ?name ## \? 
           = method (c :: <character>) 
               let code :: <integer> = as(<integer>, c);
               let vec :: <simple-object-vector> = ?name ## \-table;
               vec[code + 1]
             end; }
chars:
  { (?stuff) }
    => { concatenate(?stuff) }
  { ?stuff }
    => { ?stuff }
end macro;

define macro lexer-table-definer
  { define lexer-table ?table:name }
    => { define constant ?table :: <simple-object-vector> 
           = make-lexer-table() }
end macro;

define macro lexer-action-definer
  { define lexer-action ?table:expression on (?chars) ?body end }
    => { register-lexer-action(?table, concatenate(?chars), 
                               method (s :: <lexer-state>) ?body end) }
end macro;

define method make-boolean-table (chars :: <byte-string>)
  let table :: <simple-object-vector> 
    = make(<simple-object-vector>, size: 129, fill: #f);
  for (char :: <character> in chars)
    table[as(<integer>, char) + 1] := #t;
    table[as(<integer>, as-uppercase(char)) + 1] := #t;
  end;
  table
end method;

define method make-lexer-table ()
  make(<simple-object-vector>, size: 128)
end method;

define method register-lexer-action 
    (table :: <vector>, chars :: <byte-string>, action :: <function>)
  for (char :: <character> in chars)
    table[as(<integer>, char)] := action;
    table[as(<integer>, as-uppercase(char))] := action;
  end
end method;

// eof
