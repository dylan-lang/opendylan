Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module scepter-ir-back-end-internal
  use common-dylan, exclude: { format-to-string };
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

