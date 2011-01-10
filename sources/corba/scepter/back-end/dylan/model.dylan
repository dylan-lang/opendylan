Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <dim> (<object>)
  constant slot dim-node :: <ast-declarator>, required-init-keyword: node:;
end class;

define sealed domain make (subclass(<dim>));
define sealed domain initialize (<dim>);

define variable *node-to-dim* :: <table> = make(<table>);

define method make (class == <dim>, #key node :: <ast-declarator>)
 => (model :: <dim>)
  let model = element(*node-to-dim*, node, default: #f);
  unless (model)
    model := make-dim(node);
    *node-to-dim*[node] := model;
  end unless;
  model;
end method;

define method dim-idl-local-name (model :: <dim>)
  => (name :: <string>)
  let node :: <ast-declarator> = dim-node(model);
  let idl-local-name = declarator-local-name(node);
  identifier-label(idl-local-name);
end method;

define method dim-dylan-local-name (model :: <dim>)
 => (name :: <string>)
  map-to-dylan-name(dim-idl-local-name(model));
end method;

/* --- Not currently used
define method dim-idl-scoped-name (model :: <dim>)
 => (name :: <string>)
  apply(concatenate, map(identifier-label, declarator-scoped-name(model.dim-node)));
end method;

define method dim-dylan-scoped-name (model :: <dim>)
 => (name :: <string>)
  map-to-dylan-name(declarator-scoped-name(model.dim-node));
end method;
*/

define method dim-repository-id (model :: <dim>)
 => (id :: <string>)
  declarator-repository-id(dim-node(model));
end method;

define method before-code-emission (model :: <dim>)
 => ()
end method;

define method emit-code (back-end :: <dylan-back-end>, model :: <dim>)
 => ()
  if (dbe-emit-protocol-project?(back-end))
    let stream = dbe-protocol-stream(back-end);
    emit-protocol-code(model, stream);
    let exports = protocol-exports(model);
    do(curry(dbe-add-protocol-export, back-end), exports);
  end if;

  if (dbe-emit-shared-project?(back-end))
    let stream = dbe-shared-stream(back-end);
    emit-shared-code(model, stream);
    let exports = shared-exports(model);
    do(curry(dbe-add-shared-export, back-end), exports);
  end if;

  if (dbe-emit-stubs-project?(back-end))
    let stream = dbe-stubs-stream(back-end);
    emit-stubs-code(model, stream);
    let exports = stubs-exports(model);
    do(curry(dbe-add-stubs-export, back-end), exports);
  end if;

  if (dbe-emit-skeletons-project?(back-end))
    let stream = dbe-skeletons-stream(back-end);
    emit-skeletons-code(model, stream);
    let exports = skeletons-exports(model);
    do(curry(dbe-add-skeletons-export, back-end), exports);
  end if;
end method;

define method emit-protocol-code (model :: <dim>, stream :: <stream>)
 => ()
end method;

define method emit-shared-code (model :: <dim>, stream :: <stream>)
 => ()
end method;

define method emit-stubs-code (model :: <dim>, stream :: <stream>)
 => ()
end method;

define method emit-skeletons-code (model :: <dim>, stream :: <stream>)
 => ()
end method;

define method protocol-exports (model :: <dim>)
 => (exports :: <sequence>)
  #[];
end method;

define method shared-exports (model :: <dim>)
 => (exports :: <sequence>)
  #[];
end method;

define method stubs-exports (model :: <dim>)
 => (exports :: <sequence>)
  #[];
end method;

define method skeletons-exports (model :: <dim>)
 => (exports :: <sequence>)
  #[];
end method;

define method after-code-emission (model :: <dim>)
 => ()
end method;
