Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant objects

define class <constant-object> 
    (<variable-object>,
     <definition-object>)
end class <constant-object>;


/// Implementation

define method environment-object-type-name
    (object :: <constant-object>) => (label :: <string>)
  "Constant"
end method environment-object-type-name;

