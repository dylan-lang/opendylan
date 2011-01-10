Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-union> (<ast-structure>)
  constant slot union-discriminator-type :: <ast-concrete-type>, init-keyword: type:;
end class;

define method can-be-redefined? (union :: <ast-union>)
  #t;
end method;

define method initialize (union :: <ast-union>, #key)
  next-method();
  let type = union.union-discriminator-type;
  unless (instance?(type, <ast-concrete-type>))
    error(make(<idl-not-a-type>, declarators: vector(type)));
  end unless;
end method;

// initialize method may need to install discr type from
// something and check it is a predefined type

define method check-default-branch (union :: <ast-union>)
  for (declarator in union.scope-declarators)
    if (instance?(declarator, <ast-union-branch>))
        for (existing-label in declarator.union-branch-labels)
          if (instance?(existing-label, <ast-default-union-branch-label>))
	    error(make(<idl-duplicate-union-branch-label>, declarators: vector(union, declarator)));
	  end if;
	end for;
    end if;
  end for;
end method;

define method check-branch-by-label (union :: <ast-union>, union-branch :: <ast-union-branch>, label :: <ast-union-branch-label>)
  label.union-branch-label-value.expression-value :=
    coerce(label.union-branch-label-value, union.union-discriminator-type.predefined-type);
  for (declarator in union.scope-declarators)
    if (instance?(declarator, <ast-union-branch>))
      for (existing-label in declarator.union-branch-labels)
	if (~instance?(existing-label, <ast-default-union-branch-label>) &
	      existing-label.union-branch-label-value.expression-value =
	        label.union-branch-label-value.expression-value)
	  error(make(<idl-duplicate-union-branch-label>, declarators: vector(union, declarator)));
	end if;
      end for;
    end if;
  end for;
end method;

define method check-branch-by-enum (union :: <ast-union>, union-branch :: <ast-union-branch>, label :: <ast-union-branch-label>)
  let enum = union.union-discriminator-type;
  let value = label.union-branch-label-value;
  unless (value.expression-combinator == $symbol-combinator)
    error(make(<idl-enum-value-expected>, union: union, label: label));
  end unless;
  let declarator = resolve-scoped-name(enum, value.expression-scoped-name, reference?: #t);
  unless (declarator & (declarator.declarator-scope == enum))
    error(make(<idl-enum-value-lookup-failure>, union: union, enum: enum, declarator: declarator));
  end unless;
  for (declarator in union.scope-declarators)
    if (instance?(declarator, <ast-union-branch>))
      for (existing-label in declarator.union-branch-labels)
	if (~instance?(existing-label, <ast-default-union-branch-label>)
	      & existing-label.union-branch-label-value.expression-value =
	          label.union-branch-label-value.expression-value)
	  error(make(<idl-duplicate-union-branch-label>, declarators: vector(union, declarator)));
	end if;
      end for;
    end if;
  end for;
end method;

define method check-branch (union :: <ast-union>, union-branch :: <ast-union-branch>)
  for (label in union-branch.union-branch-labels)
    case
      (instance?(label, <ast-default-union-branch-label>)) =>
	check-default-branch(union);
      (instance?(union.union-discriminator-type, <ast-enum>)) =>
	check-branch-by-enum(union, union-branch, label);
      otherwise =>
	check-branch-by-label(union, union-branch, label);
    end case;
  end for;
end method; 

define method check-before-add (union :: <ast-union>, union-branch :: <ast-union-branch>)
  check-branch(union, union-branch);
  next-method();
end method;

define method add-type (union :: <ast-union>)
  add-declarator-to-scope(union.declarator-scope, union);
  call-add(union);
end method;

