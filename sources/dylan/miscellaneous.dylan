Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline function identity (object) => object;
  object
end function;

define inline function ignore (#rest ignored-values) => ()
  ignored-values;
  values()
end;

define inline function ignorable (#rest ignorable-values) => ()
  ignorable-values;
  values()
end;
