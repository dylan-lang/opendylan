module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define persistent-class <persistent-list> (<persistent-object>, <sequence>)
  slot persistent-head;
  slot persistent-tail;
end;

define persistent-class <persistent-pair> (<persistent-list>) end;

define persistent-class <persistent-empty-list> (<persistent-list>) end;

define method persistent-pair(connection :: <database-connection>, x, y)
  let pair = persistent-make(connection, <persistent-pair>);
  pair.persistent-head := x;
  pair.persistent-tail := y;
  pair;
end;  

define method persistent-shallow-copy
    (connection :: <database-connection>, x :: <empty-list>)
  connection.db-database.db-nil;
end;

define method persistent-shallow-copy
    (connection :: <database-connection>, x :: <pair>)
  persistent-pair
    (connection, x.head, persistent-shallow-copy(connection, x.tail));
end;

define method persistent-copy
    (connection :: <database-connection>, x :: <empty-list>)
  connection.db-database.db-nil;
end;

define method persistent-copy
    (connection :: <database-connection>, x :: <pair>)
  persistent-pair
    (connection, 
     persistent-copy(connection, x.head), persistent-copy(connection, x.tail));
end;

define method ephemeral-shallow-copy
    (connection :: <database-connection>, x :: <list>)
  as(<list>, x);
end;

define method ephemeral-copy
    (connection :: <database-connection>, x :: <list>)
  pair(ephemeral-copy(connection, x.head), ephemeral-copy(connection, x.tail));
end;

/// TYPE MAPPING

define method persistent-object-class (object :: <pair>) 
  <persistent-pair>
end;

define method persistent-object-class (object :: <empty-list>) 
  <persistent-empty-list> 
end;

define method ephemeral-object-class (object :: <persistent-pair>)
  <pair>
end;

define method ephemeral-object-class (object :: <persistent-empty-list>)
  <empty-list>
end;

/// ITERATION PROTOCOL

define method initial-state (list :: <persistent-empty-list>)
  #f;
end method;

define method initial-state (list :: <persistent-list>)
  list;
end method;

define method next-state (list :: <persistent-list>, state :: <persistent-list>)
  let maybe :: <persistent-list> = state.persistent-tail;
  if (instance?(maybe, <persistent-empty-list>))
    #f;
  else
    maybe;
  end if;
end method;

define method copy-state (list :: <persistent-list>, state :: <persistent-list>)
  state;
end method;

define method current-element
    (list :: <persistent-list>, state :: <persistent-list>)
  state.persistent-head;
end method;

define method current-element-setter
    (new-value, list :: <persistent-list>, state :: <persistent-list>)
  state.persistent-head := new-value;
end method;

// eof
