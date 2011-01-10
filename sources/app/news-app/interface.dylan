module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *query* = #f;

define method as-c-boolean (boolean :: <object>)
  if (boolean)
    1
  else
    0
  end if
end method as-c-boolean;

define method set-query (query-string :: <string>) => (c-boolean :: <integer>)
  as-c-boolean(block ()
                 *query* := parse-query(as(<byte-string>, query-string), 0);
                 #t
               exception (<error>)
                 #f
               end block)
end method set-query;

define method query-text (text :: <string>) => (c-boolean :: <integer>)
  as-c-boolean(block ()
                 scan-text(as(<byte-string>, text), *query*, 0)
               exception (<error>)
                 #f
               end block)
end method set-query;


// FFI

define C-callable-wrapper c-set-query of set-query
  parameter query-string :: <c-string>;
  result value :: <c-int>;
  c-name: "SetQuery", c-modifiers: "__stdcall";
end C-callable-wrapper;

define C-callable-wrapper c-query-text of query-text
  parameter text :: <c-string>;
  result value :: <c-int>;
  c-name: "QueryText", c-modifiers: "__stdcall";
end C-callable-wrapper;
