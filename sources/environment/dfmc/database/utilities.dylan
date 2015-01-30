Module:    dfmc-environment-database
Synopsis:  DFM compiler database utilities
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function ensure-server-database
    (server :: <server>) => (database :: <dfmc-database>)
  select (server by instance?)
    <dfmc-database>  => server;
    <project-object> => server.project-compiler-database;
    otherwise        => #f;
  end
    | error("Unexpectedly failed to find database for %s", server)
end function ensure-server-database;

// Return the appropriate environment class for a definition source form.
define inline function environment-class-for-source-form
    (server :: <server>, source-form :: <source-form>)
 => (class :: subclass(<source-form-object>))
  local method method-class
            () => (class :: subclass(<source-form-object>))
          let database = ensure-server-database(server);
          if (internal-dylan-method?(database, source-form))
            <internal-method-object>
          else
            <method-object>
          end
        end method method-class;
  select (source-form by instance?)
    //---*** andrewa: this mapping is no longer correct, we
    //---*** map <project>s to <library-object>.
    // <library-definition>                => <library-object>;
    <module-definition>                 => <module-object>;
    <macro-definition>                  => <macro-object>;
    <method-definition>                 => method-class();
    <generic-definition>                => <generic-function-object>;
    <function-definition>               => method-class();
    // Order of next two is significant
    <constant-method-definition>        => method-class();
    <constant-definition>               => <constant-object>;
    <domain-definition>                 => <domain-object>;
    <class-definition>                  => <class-object>;
    <macro-form>                        => <simple-macro-call-object>;
    <variable-definition>               =>
      if (member?(#"thread", source-form-adjectives(source-form)))
        <thread-variable-object>
      else
        <global-variable-object>
      end;
    otherwise =>
      <top-level-expression-object>;
  end
end function environment-class-for-source-form;

define sealed method make-environment-object-for-source-form
    (server :: <server>, definition :: <source-form>)
 => (object :: <environment-object>)
  let environment-class
    = environment-class-for-source-form(server, definition);
  make-environment-object(environment-class,
                          project: server.server-project,
                          compiler-object-proxy: definition)
end method make-environment-object-for-source-form;

//--- Special case 'define function' so that it doesn't look like
//--- a macro call.
define sealed method make-environment-object-for-source-form
    (server :: <server>, form :: <macro-form>)
 => (object :: <environment-object>)
  let function? = source-form-define-word(form) == #"function";
  let forms = if (function?) form.macro-form-expanded-forms end | #[];
  if (size(forms) = 1)
    make-environment-object-for-source-form(server, forms[0])
  else
    next-method()
  end
end method make-environment-object-for-source-form;

define sealed method make-environment-object-for-source-form
    (server :: <server>, definition :: <library-definition>)
 => (object :: <library-object>)
  let name = definition.library-definition-name;
  let database = server.ensure-server-database;
  let project = find-project-for-library-name(database, name);
  make-environment-object(<library-object>,
                          project: server.server-project,
                          compiler-object-proxy: project)
end method make-environment-object-for-source-form;

define class <dfmc-type-expression-object> (<complex-type-expression-object>)
end;

define method environment-object-primitive-name
  (server :: <server>, expression :: <dfmc-type-expression-object>) => (result :: false-or(<string>));
  let s :: <byte-string-stream>
    = make(<byte-string-stream>,
           contents: make(<byte-string>, size: 32), direction: #"output");
  print(expression.compiler-object-proxy, s, escape?: #f);
  s.stream-contents
end;

define function make-environment-object-for-type-expression
    (server :: <dfmc-database>, type-expression :: false-or(<type-expression>))
 => (object :: <environment-object>)
  let type-definition = type-expression-to-definition(server, type-expression);
  if (type-definition)
    make-environment-object-for-source-form
      (server.server-project, type-definition)
  else
    make-environment-object(<dfmc-type-expression-object>,
                            project: server.server-project,
                            compiler-object-proxy: type-expression)
  end
end function make-environment-object-for-type-expression;

define function find-<object>
    (server :: <dfmc-database>)
 => (definition :: <definition>)
  find-compiler-database-proxy(server, $<object>-id)
    | error("Failed to find <object>!")
end function find-<object>;

define method type-expression-to-definition
    (server :: <dfmc-database>, type-expression :: false-or(<type-expression>))
 => (object :: false-or(<source-form>))
  let context = browsing-context(server, server);
  // Arbitrarily complex type expression
  case
    type-expression == #t =>
      #f;
    type-expression == #f =>
      find-<object>(server);
    instance?(type-expression, <variable>) =>
      let definition
        = variable-active-definition(context, type-expression);
      unless (definition)
        debug-out(#"dfmc-environment-database",
                  "No active definition found for %=",
                  type-expression)
      end;
      definition;
  end case
end method type-expression-to-definition;

define method type-expression-to-definition
    (server :: <dfmc-database>, type-expression :: <class-definition>)
 => (object :: false-or(<source-form>))
  //---*** cpage: 1997.12.16  This case shouldn't occur, but used to
  //              in the past. Since there's a trivial work-around,
  //              let's do it here for the short-term until we verify
  //              that this case no longer occurs.
  debug-out(#"dfmc-environment-database",
            "make-environment-object-for-type-expression:"
              " The type expression %= is a <class-definition>"
              " when it should be a <variable>",
            type-expression);
  type-expression
end method type-expression-to-definition;

define function name-to-string
    (name :: <symbol>) => (string :: <string>)
  as-lowercase(as(<string>, name))
end function name-to-string;
