Module: dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module scepter-dump-back-end
  use date;
  use generic-arithmetic-common-dylan,
    exclude: { format-to-string };
  use format;
  use streams;
  use file-system;
  use scepter-back-end;
  use scepter-front-end;
  use scepter-ast;
end module;
