Module:   dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library file-reader
  use dylan;
  use functional-extensions;
  use locators;
  export file-reader;
end library;

define module file-reader
  use dylan;
  use stream;
  use infix-reader;
  use functional-extensions;
  use locators;
  use translator,
        import: { add-feature,
                  file-header-default-module-name,
                  file-header-default-module-name-setter };
  use internal,
        import: { read-file-header, maybe-read-file-header };
  export
    read-da-big-form, read-source-file, read-source-file-header,
    open-source-file, close-source-file, read-form;
end module;
