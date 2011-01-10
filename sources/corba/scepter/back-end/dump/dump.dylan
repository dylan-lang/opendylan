Module:    scepter-dump-back-end
Author:    Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// DUMP BACKEND

define scepter-back-end <dump-back-end>
  command-line-syntax: "dump",
  filename-extension: "new"
  constant slot dump-scopes :: <deque> = make(<deque>);
end scepter-back-end;

// DUMP-BACK-END
//
// Make dump-back-end a singleton so that error reporting in front
// end can use same one efficiently.

define variable *dump-back-end* :: false-or(<dump-back-end>) = #f;

define method make (back-end-class == <dump-back-end>, #key, #all-keys)
 => (back-end :: <dump-back-end>)
  *dump-back-end*
  | (*dump-back-end* := next-method());
end method;

define constant $dump-version = "1.0";

define method scepter-back-end-banner (back-end :: subclass(<dump-back-end>), stream :: <stream>)
 => ()
  let scepter = get-scepter();
  format(stream, "\n%s, dump version %s", scepter.scepter-program-name, $dump-version);
end method;

define method scepter-back-end-emit (back-end :: <dump-back-end>, root :: <ast-root>, front-end :: <scepter-front-end>, source :: <scepter-source>)
 => (result :: <scepter-back-end-result>)
  let scepter = back-end.scepter-back-end-scepter;
  if (scepter.scepter-write-to-standard-output?)
    dump-definition(back-end, root, scepter.scepter-output);
  else
    let filename = concatenate(scepter-source-base-name(source), ".new");
    with-open-file(stream = filename, direction: #"output")
      let old-idl-output = scepter.scepter-output;
      block ()
        scepter.scepter-output := make(<indenting-stream>, inner-stream: stream);
        dump-definition(back-end, root, scepter.scepter-output);
      cleanup
        close(scepter.scepter-output);
        scepter.scepter-output := old-idl-output;
      end block;
    end with-open-file;
  end if;
  make(<scepter-back-end-result>, success?: #t);
end method;
  
// DUMP-DEFINITION

define method dump-definition (back-end :: <dump-back-end>, object :: <object>, stream :: <stream>, #key last? :: <boolean> = #f)
  dump-newline(back-end, object, stream);
  dump-before-name(back-end, object, stream);
  dump-name(back-end, object, stream);
  dump-after-name(back-end, object, stream);
  with-indented-body (stream)
    dump-open-bracket(back-end, object, stream);
    dump-body(back-end, object, stream);
    dump-close-bracket(back-end, object, stream);
  end with-indented-body;
  dump-after-body(back-end, object, stream);
  dump-semicolon(back-end, object, stream, last?: last?);
end method;

define method dump-before-name (back-end :: <dump-back-end>, object :: <object>, stream  :: <stream>)
end method;

define method dump-name (back-end :: <dump-back-end>, object :: <object>, stream  :: <stream>)
end method;

define method dump-after-name (back-end :: <dump-back-end>, object :: <object>, stream  :: <stream>)
end method;

define method dump-open-bracket (back-end :: <dump-back-end>, object :: <object>, stream  :: <stream>)
end method;

define method dump-body (back-end :: <dump-back-end>, object :: <object>, stream  :: <stream>)
end method;

define method dump-after-body (back-end :: <dump-back-end>, object :: <object>, stream  :: <stream>)
end method;

define method dump-close-bracket (back-end :: <dump-back-end>, object :: <object>, stream  :: <stream>)
end method;

define method dump-newline (back-end :: <dump-back-end>, x :: <object>, stream :: <stream>)
  format(stream, "\n");
end method;

define method dump-semicolon (back-end :: <dump-back-end>, x :: <object>, stream :: <stream>, #key last? :: <boolean> = #f)
  format(stream, ";");
end method;

define method dump-open-bracket (back-end :: <dump-back-end>, scope :: <scope>, stream :: <stream>)
  format(stream, " {");
end method;

define method dump-close-bracket (back-end :: <dump-back-end>, scope :: <scope>, stream :: <stream>)
  format(stream, "\n}");
end method;


// MISCELLANEOUS

define method dump-name (back-end :: <dump-back-end>, name :: <ast-identifier>, stream :: <stream>)
  format(stream, "%s", name.identifier-label);
end method;

define method dump-name (back-end :: <dump-back-end>, name :: <ast-scoped-name>, stream :: <stream>)
  print-separated-collection(name, "::", stream, printer: curry(dump-name, back-end));
end method;

// inefficient at the moment
define method dump-scoped-name (back-end :: <dump-back-end>, ast-object :: <object>, name :: <ast-scoped-name>, stream :: <stream>)
  block (return)
    for (i from name.size - 1 to 0 by -1)
      let found = resolve-scoped-name(back-end.dump-scopes.first, name, start: i);
      if (found & (found == ast-object.full-definition))
        dump-name(back-end, copy-sequence(name, start: i), stream);
        return();
      end if;
    end for;
    dump-name(back-end, name, stream);
  end block;
end method;

// ARGUMENT

define method dump-before-name (back-end :: <dump-back-end>, argument :: <ast-argument>, stream :: <stream>)
  format(stream, "%s ", argument.argument-direction.argument-direction-name);
  next-method();
end method;

// ARRAY

define method dump-name (back-end :: <dump-back-end>, array :: <ast-array>, stream :: <stream>)
  dump-name(back-end, array.array-base-type, stream);
end method;

define method dump-after-name (back-end :: <dump-back-end>, array :: <ast-array>, stream :: <stream>)
  for (expression in array.array-dimensions)
    format(stream, "[");
    dump-name(back-end, expression, stream);
    format(stream, "]");
  end for;
end method;

// ATTRIBUTE

define method dump-before-name (back-end :: <dump-back-end>, attribute :: <ast-attribute>, stream :: <stream>)
  if (attribute.attribute-read-only?)
    format(stream, "readonly ");
  end if;
  format(stream, "attribute ");
  next-method();
end method;

// CONSTANT

define method dump-before-name (back-end :: <dump-back-end>, ast-constant :: <ast-constant>, stream :: <stream>)
  format(stream, "const ");
  dump-name(back-end, ast-constant.constant-expression-type, stream);
  format(stream, " ");
end method;

define method dump-after-name (back-end :: <dump-back-end>, ast-constant :: <ast-constant>, stream :: <stream>)
  format(stream, " = ");
  dump-name(back-end, ast-constant.constant-value, stream);
end method;

// DECLARATOR

define method dump-name (back-end :: <dump-back-end>, declarator :: <ast-declarator>, stream  :: <stream>)
  dump-scoped-name(back-end, declarator, declarator.declarator-scoped-name, stream);
end method;

// ENUM

define method dump-definition (back-end :: <dump-back-end>, enum-value :: <ast-enum-value>, stream :: <stream>, #key last? :: <boolean>)
end method;

define method dump-before-name (back-end :: <dump-back-end>, enum :: <ast-enum>, stream :: <stream>)
  format(stream, "enum ");
end method;

define method dump-body (back-end :: <dump-back-end>, enum :: <ast-enum>, stream :: <stream>)
  push(back-end.dump-scopes, enum);
  print-separated-collection(enum.scope-declarators,
                             ", ",
                             stream,
			     printer: curry(dump-name, back-end));
  pop(back-end.dump-scopes);
end method;

define method dump-close-bracket (back-end :: <dump-back-end>, enum :: <ast-enum>, stream :: <stream>)
  format(stream, "}");
end method;

// EXCEPTION

define method dump-before-name (back-end :: <dump-back-end>, excep :: <ast-exception>, stream :: <stream>)
  => ()
  format(stream, "exception ");
end method;

// EXPRESSION

define method dump-name (back-end :: <dump-back-end>, ast-expression :: <ast-expression>, stream :: <stream>)
  => ()
    dump-expression(back-end, ast-expression, ast-expression.expression-combinator, stream);
end method;

define method dump-expression (back-end :: <dump-back-end>, 
				 ast-expression :: <ast-expression>,
                               expression-combinator :: <binary-expression-combinator>,
                               stream :: <stream>)
  let left-sub = ast-expression.left-subexpression;
  if (left-sub)
    dump-name(back-end, left-sub, stream);
  end if;
  format(stream, " %s ", expression-combinator.combinator-name);
  let right-sub = ast-expression.right-subexpression;
  if (right-sub)
    dump-name(back-end, right-sub, stream);
  end if;
end method;

define method dump-expression (back-end :: <dump-back-end>,
				 ast-expression :: <ast-expression>,
                               expression-combinator :: <unary-expression-combinator>,
                               stream :: <stream>)
  format(stream, "%s", expression-combinator.combinator-name);
  dump-name(back-end, ast-expression.right-subexpression, stream);
end method;

define method dump-expression (back-end :: <dump-back-end>, 
				 ast-expression :: <ast-expression>,
                               expression-combinator :: <symbol-combinator>,
                               stream :: <stream>)
  let scoped-name = ast-expression.expression-scoped-name;
  let declarator = resolve-scoped-name(back-end.dump-scopes.first, scoped-name, start: size(scoped-name) - 1);
  if (declarator)
    dump-name(back-end, scoped-name.last, stream);
  else
    dump-scoped-name(back-end, ast-expression, scoped-name, stream);
  end if;
end method;

define method dump-expression (back-end :: <dump-back-end>, 
			       ast-expression :: <ast-expression>,
                               expression-combinator :: <no-combinator>,
                               stream :: <stream>)
  if (ast-expression.expression-value)
    dump-expression-value(back-end, ast-expression, ast-expression.expression-type, stream);
  end if;
end method;

define method dump-expression-value (back-end :: <dump-back-end>, 
				     ast-expression :: <ast-expression>,
                                     expression-type :: <object>,
                                     stream :: <stream>)
  format(stream, "%=", ast-expression.expression-value);
end method;

define method dump-expression-value (back-end :: <dump-back-end>, 
				     ast-expression :: <ast-expression>,
                                     expression-type :: <boolean-idl-type>,
                                     stream :: <stream>)
  format(stream,
         "%s",
         select (ast-expression.expression-value)
           #t => "TRUE";
           #f => "FALSE";
         end select);
end method;

define method dump-expression-value (back-end :: <dump-back-end>,
				     ast-expression :: <ast-expression>,
                                     expression-type :: <any-idl-type>,
                                     stream :: <stream>)
end method;

define method dump-expression-value (back-end :: <dump-back-end>,
				     ast-expression :: <ast-expression>,
                                     expression-type :: <none-idl-type>,
                                     stream :: <stream>)
end method;

define method dump-expression-value (back-end :: <dump-back-end>,
				     ast-expression :: <ast-expression>,
                                     expression-type :: <void-idl-type>,
                                     stream :: <stream>)
end method;

// FIELD

define method dump-before-name (back-end :: <dump-back-end>, field :: <ast-field>, stream :: <stream>)
  dump-name(back-end, field.field-type, stream);
  format(stream, " ");
end method;

define method dump-after-name (back-end :: <dump-back-end>, field :: <ast-field>, stream :: <stream>)
  select (field.field-type by instance?)
    <ast-array> =>
      dump-after-name(back-end, field.field-type, stream);
    otherwise => #f;
  end select;
end method;

// INTERFACE

define method dump-before-name (back-end :: <dump-back-end>, interface :: <ast-interface>, stream :: <stream>)
  format(stream, "interface ");
end method;

define method dump-after-name (back-end :: <dump-back-end>, interface :: <ast-interface>, stream :: <stream>)
  unless (empty?(interface.interface-inherits))
    format(stream, " : ");
    for (inherited-interface in interface.interface-inherits,
         i from 1 to interface.interface-inherits.size)
      dump-name(back-end, inherited-interface, stream);
      unless (i == interface.interface-inherits.size)
	format(stream, ", ");
      end unless;
    end for;
  end unless;
end method;

// INTERFACE-FORWARD

define method dump-before-name (back-end :: <dump-back-end>, interface :: <ast-forward-interface>, stream :: <stream>)
  format(stream, "interface ");
end method;

// MODULE

define method dump-before-name (back-end :: <dump-back-end>, module :: <ast-module>, stream :: <stream>)
  format(stream, "module ");
end method;

// OPERATION

define method dump-before-name (back-end :: <dump-back-end>, operation :: <ast-operation>, stream :: <stream>)
  unless (operation.operation-flag == $no-operation-flag)
    format(stream, "%s ", operation.operation-flag.operation-flag-name);
  end unless;
  dump-name(back-end, operation.operation-return-type, stream);
  format(stream, " ");
end method;

define method dump-open-bracket (back-end :: <dump-back-end>, operation :: <ast-operation>, stream :: <stream>)
  format(stream, "(");
end method;

define method dump-close-bracket (back-end :: <dump-back-end>, operation :: <ast-operation>, stream :: <stream>)
  format(stream, ")");
end method;

define method dump-newline (back-end :: <dump-back-end>, arg :: <ast-argument>, stream :: <stream>)
end method;

define method dump-semicolon (back-end :: <dump-back-end>, arg :: <ast-argument>, stream :: <stream>, #key last? :: <boolean> = #f)
  unless (last?)
    format(stream, ", ");
  end unless;
end method;

define method dump-after-body (back-end :: <dump-back-end>, operation :: <ast-operation>, stream :: <stream>)
  let exceptions = operation.operation-exceptions;
  if (exceptions & ~exceptions.empty?)
    format(stream, " raises(");
    print-separated-collection(operation.operation-exceptions, ", ", stream, printer: curry(dump-name, back-end));
    format(stream, ")");
  end if;
  let context = operation.operation-context;
  if (context & context.empty?)
    format(stream, " context(");
    print-separated-collection(operation.operation-context, ", ", stream);
    format(stream, ")");
  end if;
end method;

// PREDEFINED-TYPE

define method dump-definition (back-end :: <dump-back-end>, type :: <ast-predefined-type>,  stream :: <stream>, #key last? :: <boolean>)
end method;

// ROOT (module)

define method dump-definition (back-end :: <dump-back-end>, root :: <ast-root>,   stream :: <stream>, #key last? :: <boolean>)
  dump-body(back-end, root, stream);
end method;

// SCOPE

define method dump-body (back-end :: <dump-back-end>, scope :: <scope>, stream :: <stream>)
  push(back-end.dump-scopes, scope);
  for (declarator in scope.scope-declarators,
       i from 1 to scope.scope-declarators.size)
    when (emit-declarator?(back-end, declarator))
      dump-definition(back-end, declarator, stream, last?: (i == scope.scope-declarators.size));
    end when;
  end for;
  pop(back-end.dump-scopes);
end method;

// SEQUENCE

define method dump-name (back-end :: <dump-back-end>, sequence :: <ast-sequence>, stream :: <stream>)
  format(stream, "sequence <");
  dump-name(back-end, sequence.sequence-base-type, stream);
  if (sequence.sequence-max-size)
    format(stream, ", ");
    dump-name(back-end, sequence.sequence-max-size, stream);
  end if;
  format(stream, ">");
end method;

// STRING

define method dump-name (back-end :: <dump-back-end>, string :: <ast-string>, stream :: <stream>)
  format(stream, "string");
  if (string.string-max-size)
    format(stream, " <");
    dump-name(back-end, string.string-max-size, stream);
    format(stream, ">");
  end if;
end method;

// STRUCT

define method dump-before-name (back-end :: <dump-back-end>, structure :: <ast-structure>, stream :: <stream>)
  format(stream, "struct ");
end method;

// TYPE

// TYPEDEF

define method dump-before-name (back-end :: <dump-back-end>, typedef :: <ast-typedef>, stream :: <stream>)
  format(stream, "typedef ");
  dump-name(back-end, typedef.typedef-base-type, stream);
  format(stream, " ");
end method;

define method dump-after-name (back-end :: <dump-back-end>, typedef :: <ast-typedef>, stream :: <stream>)
  select (typedef.typedef-base-type by instance?)
    <ast-array> =>
      dump-after-name(back-end, typedef.typedef-base-type, stream);
    otherwise => #f;
  end select;
end method;

// UNION

define method dump-before-name (back-end :: <dump-back-end>, union :: <ast-union>, stream :: <stream>)
  format(stream, "union ");
end method;

define method dump-after-name (back-end :: <dump-back-end>, union :: <ast-union>, stream :: <stream>)
  format(stream, " switch (");
  dump-name(back-end, union.union-discriminator-type, stream);
  format(stream, ")");
end method;

// UNION-BRANCH

define method dump-before-name (back-end :: <dump-back-end>, union-branch :: <ast-union-branch>, stream :: <stream>)
  for (label in union-branch.union-branch-labels)
    select (label by instance?)
      <ast-union-branch-label> =>
        format(stream, "case ");
        dump-name(back-end, label.union-branch-label-value, stream);
      <ast-default-union-branch-label> =>
        format(stream, "default");
    end select;
    format(stream, ": ");
  end for;
  next-method();
end method;

