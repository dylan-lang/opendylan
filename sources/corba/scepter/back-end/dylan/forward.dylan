Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *forward* :: <stretchy-vector> = make(<stretchy-vector>);

/*
define class <dim-forward> (<dim-named-type>, <environment-entry>)
end class;

define method initialize (forward :: <dim-forward>, #key node :: <ast-forward-interface>)
  next-method();
  forward.environment-entry-name := dim-dylan-local-name(forward);
  forward.environment-entry-value := make(<environment>);
end method;

define method forward-model-class-name (forward :: <dim-forward>)
 => (name :: <string>)
  let idl-scoped-name = declarator-scoped-name(forward.dim-node);
  map-to-dylan-class-name(idl-scoped-name);
end method;
*/

define method emit-ast-node (back-end :: <dylan-back-end>, forward :: <ast-forward-interface>, #key last? :: <boolean>)
 => ()
  let interface-class-name = map-to-dylan-class-name(declarator-scoped-name(forward));
  unless (member?(interface-class-name, *forward*, test: \=))
    add!(*forward*, interface-class-name);
    if (dbe-emit-shared-project?(back-end))
      emit-forward-interface-typecode-constant-definition(dbe-shared-stream(back-end), forward);
    end if;
  end unless;
end method;

define method forward-interface-typecode-constant-name (forward :: <ast-forward-interface>)
 => (name :: <string>)
  map-to-dylan-name(declarator-scoped-name(forward),
                    prefix: $dylan-constant-name-prefix,
                    suffix: "-typecode");
end method;

define method emit-forward-interface-typecode-constant-definition (stream :: <stream>, forward :: <ast-forward-interface>)
 => ()
  format(stream, "define constant %s = make(<object-reference-typecode>, type: %s, name: %=, repository-id: %=);\n\n",
	 forward-interface-typecode-constant-name(forward),
	 map-to-dylan-class-name(declarator-scoped-name(forward)),
	 identifier-label(declarator-local-name(forward)),
	 declarator-repository-id(forward));
end method;
