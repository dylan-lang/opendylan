Module: dylan-user
Author: Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* start with a library definition which is very similar to that in dfmc-browser-support */

define library dfmc-harp-browser-support
  use functional-dylan;
  use source-records;
  use dfmc-core;
  use dfmc-namespace;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-management;
  use dfmc-back-end;
  use dfmc-browser-support;
  use harp;
end library;

define module harp-browser-used-modules
  use source-records, export: all;
  use dfmc-core, export: all;
  use dfmc-reader, export: all;
  use dfmc-conversion, export: all;
  use dfmc-management, export: all;
  use dfmc-back-end, export: all;
end module;


define module dfmc-harp-browser-support
  use functional-dylan;
  use dylan-extensions, import: {case-insensitive-equal};
  use dfmc-imports, 
    exclude: { source-record-start-line, source-record-end-line };
  use dfmc-namespace;
  use dfmc-derived-information;
  use dfmc-browser-back-end;
  // Use prefix to avoid conflicts with interface names.
  use harp-browser-used-modules, prefix: "dfmc-";
  // Rename to avoid conflicts with interface names.
  use harp-for-extenders, 
    rename: { <compiled-lambda> => <harp-compiled-lambda>,
              variable-name => harp-variable-name };
end module;
