Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library grammar-compiler
  use functional-dylan;
  export grammar-compiler;
end;

define module grammar-compiler
  use functional-dylan;

  export compile-grammar-rules;

  export <grammar>,
    grammar-rules,
    grammar-error-rules,
    grammar-terminals,
    grammar-rule-reduction-table,
    grammar-action-table,
    grammar-goto-table;

  export <grammar-conflict>,
    grammar-conflict-terminal,
    grammar-conflict-action-1,
    grammar-conflict-rule-1,
    grammar-conflict-position-1,
    grammar-conflict-action-2,
    grammar-conflict-rule-2,
    grammar-conflict-position-2;
end;
