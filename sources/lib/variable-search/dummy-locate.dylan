module: variable-search
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method locate-variable (o :: <object>)
 => (variable-encoding, module-encoding, library-encoding)
  error("Failed to locate a variable binding for %=", o);
end method;
