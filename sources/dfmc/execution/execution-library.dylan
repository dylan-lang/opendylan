module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-execution
  use dylan;
  use common-dylan;
  use variable-search;
  use system;
  use release-info;
  use dfmc-core;
  use dfmc-back-end;
  use dfmc-optimization;
  // Use the dfmc project-group
  use dfmc-management;

  // HACK: TEMPORARY
  // use dylan-script;
  use projects;

  export dfmc-execution, dfmc-runtime-execution;
end library;

define module dfmc-execution
  use common-dylan;
  use dfmc-core, export: { eval, &eval };
  use dfmc-imports;
  use dfmc-optimization;
  use dfmc-back-end;
end module;

define module dfmc-runtime-execution
  use dylan;
  use common-dylan;
  use dylan-internal;
  use dylan-extensions,
    exclude: { <ordered-object-set>, <ordered-object-table>, home-library },
    rename:  { namespace-name => library-name };
  use dylan-hygiene-glitches;
  use dylan-primitives;
  use operating-system;
  use release-info;
  use threads;
  use variable-search;
  use dfmc-core,
    exclude: { keyword-specifiers, xep, xep-setter, iep, closure-offset,
               <namespace>, <library>, <module> };
  use dfmc-imports;
  use dfmc-optimization;
  use dfmc-back-end;
  use dfmc-management,
    export: { interpret-top-level-form, unregister-interpreter-transaction };
  use projects,
    exclude: { load-library };

  export
    interpreter-transaction-value;
end module;
