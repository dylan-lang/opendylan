Module:    dylan-user
Language:  infix-dylan
Synopsis:  Define the In-Dylan Virtual Machine (IDVM) loader
Author:    Eliot Miranda, Tony Mann, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library idvm-loader
  use dylan;
  use equal-table;
  use variable-search;
  use idvm-namespace;
  //use timers;
  use simple-streams;
  use simple-format;
  use locators;
  use streams;
  use doss;
  use idvm;

  export idvm-loader;

end library idvm-loader;

define module idvm-loader
  use dylan;
  use simple-streams;
  use simple-format;
  use locators;
  use streams;
  use idvm-namespace, export: { install-constant, install-variable, install-variable-reader };
  use variable-search;
  use doss;
  use equal-table;
  use idvm;

  export
   build-vm-method,
   build-vm-closure,
   build-vm-generic-function,
   build-vm-class,
   build-vm-singleton,

   lookup-constant-definer,
   lookup-variable-reader,
   lookup-variable-writer,
   lookup-variable-definer,
   lookup-mangled,

   load-idvm-code;

end module idvm-loader;
