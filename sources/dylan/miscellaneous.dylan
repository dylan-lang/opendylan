Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline constant identity
  = method (object) => object;
      object
    end method;

define inline constant ignore =
    method (#rest ignored-values) => ()
      ignored-values;
      values()
    end;

define inline constant ignorable =
    method (#rest ignorable-values) => ()
      ignorable-values;
      values()
    end;
