Module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <&values> (<object>) end;

define class <&static-values> (<&values>)
  constant slot &values-model-objects,
    required-init-keyword: model-objects:;
end class;

/*
define class <&dynamic-values> (<&values>)
  constant slot &values-model-objects,
    required-init-keyword: model-objects:;
end class;

define method &values (#rest vals) => (vals :: <&values>)
  make(<&static-values>, model-objects: vals);
end method;

define method &values-element
    (vals :: <&values>, n :: <integer>) => (model)
  &values-model-objects(vals)[n]
end method;
*/
