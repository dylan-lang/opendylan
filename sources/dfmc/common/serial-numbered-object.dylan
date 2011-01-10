module: dfmc-common
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// I suppose this could be an attribute of the <library-description>.
define variable *object-serial-number* :: <integer> = -1;

define abstract compiler-open class <serial-numbered-object> (<object>)
  constant slot object-serial-number :: limited(<integer>, min: 0)
    = (*object-serial-number* := *object-serial-number* + 1);
end class;
