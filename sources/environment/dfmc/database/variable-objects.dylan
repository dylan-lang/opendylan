Module:    dfmc-environment-database
Synopsis:  DFM compiler variable information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Variable objects

define sealed method variable-type
    (server :: <dfmc-database>, variable :: <variable-object>)
 => (type :: false-or(<environment-object>))
  let definition = variable.compiler-object-proxy;
  let variable = definition.source-form-variable;
  let type = variable & source-form-variable-type(definition, variable);
  type & make-environment-object-for-type-expression(server, type)
end method variable-type;
