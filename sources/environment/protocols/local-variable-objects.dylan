Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Local variable objects

define class <local-variable-object> (<variable-object>)
end class <local-variable-object>;


/// Implementation

define method environment-object-type-name
    (object :: <local-variable-object>) => (label :: <string>)
  "Local variable"
end method environment-object-type-name;

