Module:    dylan-user
Author:    Gary Palter
Synopsis:  License validator for Functional Developer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library licensing
  use functional-dylan;
  use system;
  use c-ffi;
  use release-info;
  export licensing;
end library licensing;

define module licensing
  use functional-dylan;
  use simple-format;
  use date;
  use operating-system;
  use c-ffi;
  use settings;
  use settings-internals;
  use release-info;
  export 
    validate-license,
    <license-validation-failure>,
    license-info,
    unregistered-products,
    register-product;
end module licensing;
