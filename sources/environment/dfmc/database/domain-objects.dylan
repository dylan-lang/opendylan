Module:    dfmc-environment-database
Synopsis:  DFM compiler domain information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method domain-specializers
    (server :: <dfmc-database>, object :: <domain-object>)
 => (specializers :: <sequence>)
  let domain-definition :: <domain-definition> = object.compiler-object-proxy;
  let types = domain-definition.domain-definition-domain-types;
  map(curry(make-environment-object-for-type-expression, server), types)
end method domain-specializers;
