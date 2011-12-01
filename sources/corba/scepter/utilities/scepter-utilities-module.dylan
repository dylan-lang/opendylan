Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module scepter-utilities
  use generic-arithmetic-common-dylan, exclude: { <union>, format-to-string };
  use standard-io;
  use format;
  use streams;

  export          // from misc.dylan
    find,
    print-separated-collection;

  export          // from indenting.dylan
    \with-indented-body,
    <indenting-stream>,
    indent+,
    indent-;

end module;
