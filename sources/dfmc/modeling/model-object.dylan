Module: dfmc-modeling
Author: Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <virtual-object> (<object>) end;

define abstract compiler-open class <heap-deferred-model> (<model-properties>)
end;

//// Basic object structure, allocation, and access.

define generic &object-class (object) => (class);
define generic ^object-class (object) => (class);

define inline method ^object-class (object) => (class)
  &object-class(object)
end method;


define method binding-name (object)
  let var = model-variable-name(object);
  if (var)
    // TODO: This doesn't work for multi-variable define constant/variable.
    var.fragment-identifier
  end
end method;
