module:    dylan-user
Synopsis:  The module definition for the HARP-CG-BACK-END module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library harp-cg-back-end
  use common-dylan;
  use dfmc-back-end-protocol;
  use collections;
  export harp-cg-back-end;
end library harp-cg-back-end;

define module harp-cg-back-end
  use common-dylan;
  use dfmc-back-end-protocol, 
    import: {<back-end>, <local-variable>, <lambda-compiled-data>},
    export: all;
  use table-extensions, import: {<string-table>};
  export
    <harp-cg-back-end>,
    <harp-cg-variables>,
    cg-variables, cg-variables-setter,
    current-lambda, current-lambda-setter,
    current-scl, current-scl-setter,
    current-parameters, current-parameters-setter,
    stack-shift, stack-shift-setter, 
    args-to-be-dropped, args-to-be-dropped-setter,
    count-vreg, count-vreg-setter,
    next-methods-vreg, next-methods-vreg-setter,
    result-vreg, result-vreg-setter,
    function-vreg, function-vreg-setter,
    exit-tag, exit-tag-setter,
    imports, imports-setter,
    runtime-references, runtime-references-setter,
    cg-temporaries, cg-temporaries-setter,
    tags, tags-setter,
    cg-references, cg-references-setter,
    model-references, model-references-setter,
    cg-multiple-values, cg-multiple-values-setter,
    required-multiple-values, required-multiple-values-setter;
end module harp-cg-back-end;
