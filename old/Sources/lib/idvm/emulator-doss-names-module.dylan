Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module doss-names
  use dylan;
  use internal, import: {print, output, *standard-output*, format};
  use format, exclude: {format};
  use streams;
  use variable-search;
  use doss;
  use equal-table;
  use IDVM;
  use IDVM-loader,
     export: {install-constant, install-variable, install-variable-reader};

  export
    \constant-definer,
    \variable-definer;

end module doss-names;
