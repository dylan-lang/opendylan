module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <persistent-vector> 
    (<persistent-object>, <flat-sequence>, <vector>) // !@#$ KLUDGE FOR EMULATOR
end;

define method make (class == <persistent-vector>, #rest all-keys)
  apply(make, <persistent-simple-object-vector>, all-keys);
end;

define method persistent-shallow-copy
    (connection :: <database-connection>, x :: <sequence>)
  let vector = 
    persistent-make(connection, persistent-object-class(x), size: x.size);
  for (i from 0 below x.size)
    vector[i] := x[i];
  end;
  vector
end;

define method persistent-copy
    (connection :: <database-connection>, x :: <sequence>) // !@#$ EMULATOR KLUDGE
  persistent-shallow-copy(connection, x); // NYI
end;

define method ephemeral-shallow-copy
    (connection :: <database-connection>, x :: <persistent-vector>)
  as(ephemeral-object-class(x), x);
end;

define method ephemeral-copy
    (connection :: <database-connection>, x :: <persistent-vector>)
  ephemeral-shallow-copy(connection, x); // NYI
end;

/// <PERSISTENT-SIMPLE-OBJECT-VECTOR>

define persistent-class <persistent-simple-object-vector> (<persistent-vector>)
  repeated slot slot-element;
end;

define method element 
    (vector :: <persistent-simple-object-vector>, index :: <integer>, 
     #key default = unsupplied())
  slot-element(vector, index);
end;            

define method element-setter
    (new-value, vector :: <persistent-simple-object-vector>, index :: <integer>)
  slot-element(vector, index) := new-value;
end;            

/// TYPE MAPPING

define method persistent-object-class (object :: <simple-object-vector>)
  <persistent-simple-object-vector>
end;

define method ephemeral-object-class (object :: <persistent-simple-object-vector>)
  <simple-object-vector>
end;

// eof
