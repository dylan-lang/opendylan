Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Foreign objects

define class <foreign-object> (<application-code-object>)
end class <foreign-object>;


/// Implementation

define method environment-object-type-name
    (object :: <foreign-object>) => (label :: <string>)
  "Foreign object"
end method environment-object-type-name;
