module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-core
  use dylan;
  use dfmc-common, export: { dfmc-imports };
  use dfmc-flow-graph;
  use dfmc-definitions;
  use dfmc-modeling;
  use dfmc-namespace;
  use dfmc-conditions;
  export dfmc-core;
end library;

define module dfmc-core
  use dylan;
  use dfmc-common, export: all;
  use dfmc-flow-graph, export: all;
  use dfmc-definitions, export: all;
  use dfmc-modeling, export: all;
  use dfmc-namespace, export: all;
  use dfmc-conditions, export: all;
end module;

