Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <ast-generic-union-branch-label> (<object>)
end class;

// value slot should be called expression?

define class <ast-union-branch-label> (<ast-generic-union-branch-label>)
  constant slot union-branch-label-value :: false-or(<ast-expression>) = #f, init-keyword: value:;
end class;

define class <ast-default-union-branch-label> (<ast-generic-union-branch-label>)
end class;

define method initialize (union-branch-label :: <ast-union-branch-label>, #key)
  next-method();
  let branch-value = union-branch-label.union-branch-label-value;
  if (branch-value)
    branch-value.expression-value :=
      evaluate(branch-value, $constant-evaluation-kind);
  end if;
end method;

