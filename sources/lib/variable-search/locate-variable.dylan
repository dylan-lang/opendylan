module: variable-search
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method locate-variable (o)
 => (variable-encoding, module-encoding, library-encoding);
  error("Failed to locate a variable binding for %=", o);
end method;

define method locate-variable (o :: <class>)
 => (variable-encoding, module-encoding, library-encoding);
  let variable-name = copy-sequence(debug-name(o));
  let module        = class-module(o);
  let module-name   = namespace-name(module);
  let library       = home-library(module);
  let library-name  = namespace-name(library);
  values(variable-name, module-name, library-name)
end method;
