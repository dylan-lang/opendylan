module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define persistent-class <persistent-byte-string> (<persistent-vector>, <string>)
  repeated slot slot-element :: <byte-character>;
end;

define method element 
    (vector :: <persistent-byte-string>, index :: <integer>, 
     #key default = unsupplied())
  slot-element(vector, index);
end;            

define method element-setter
    (new-value :: <byte-character>, 
     vector :: <persistent-byte-string>, index :: <integer>)
  slot-element(vector, index) := new-value;
end;            

/// TYPE MAPPING

define method persistent-object-class (object :: <byte-string>)
  <persistent-byte-string>
end;

define method ephemeral-object-class (object :: <persistent-byte-string>)
  <byte-string>
end;

// eof
