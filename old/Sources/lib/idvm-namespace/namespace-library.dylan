Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library idvm-namespace
  use dylan;
  use simple-streams;
  use equal-table;

  export idvm-namespace;

end library idvm-namespace;



define module idvm-namespace
  use dylan;
  use simple-streams;
  use equal-table;

  export

    *namespace-debug-print*,
    query-error,

    *idvm-library-namespace*, idvm-namespace, *current-namespace*,
    <&object>,
    &value, &getter, &setter,
    &value-setter, &getter-setter, &setter-setter,

    install-library, install-module, install-constant, install-variable, install-variable-reader,

    lookup-namespace, lookup-variable, lookup-constant,

    shes-not-there,

    library-namer, module-namer, constant-namer, variable-namer;

end module idvm-namespace;
