Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module scepter-ir-back-end-internal
  use functional-dylan;
  use dylan-orb;
  use format;
  use streams;
  use format-out;
  use scepter-back-end;
  use scepter-front-end;
  use scepter-ast;

  export
    <ir-back-end>,
    ir-back-end-repository,
    ir-back-end-repository-setter;

  export
    load-definition,
    create-definition-in-container,
    get-primitive-type;
end module;

