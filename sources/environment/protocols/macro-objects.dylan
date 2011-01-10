Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macro objects

define class <macro-object> (<definition-object>)
end class <macro-object>;

//---*** What can you do with a macro object?


/// Implementation

define method environment-object-type-name
    (object :: <macro-object>) => (label :: <string>)
  "Macro"
end method environment-object-type-name;

