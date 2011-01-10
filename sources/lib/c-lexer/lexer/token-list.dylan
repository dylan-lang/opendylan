Module: c-lexer-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <token-list> (<t-list>) end;

define method print-object 
    (the-token-list :: <token-list>, the-stream :: <stream>) => ();
  if (empty?(the-token-list))
    format(the-stream, "(empty <token-list>)");
  else
    format(the-stream, "(<token-list> of (");
    print-separated-sequence(the-token-list, the-stream, ", ",
			     map-function: lexer-string);
    format(the-stream, "))");
  end if;
end method;

define method copy-token-list 
    (source-list :: <token-list>, 
     #key source-line: the-source-line :: false-or(<integer>) = #f)
 => (copied-list :: <token-list>)
  let copied-list = make(<token-list>);
  for (source-token in source-list)
    push-last(copied-list, copy-token(source-token, 
				      source-line: the-source-line));
  end for;
  copied-list
end method;

