Module:    dylan-user
Synopsis:  Visual SourceSafe backend for Dylan environment
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module source-control-vss-backend
  use functional-dylan;
  use simple-format;
  use machine-words,
    import: { %logior };
  use date;
  use locators;
  use operating-system;
  use settings;
  use settings-internals,
    import: { initialize-settings, settings-handle };
  use commands;
  use c-ffi;
  use win32-registry;
  use ole-automation;
  use SourceSafeTypeLib;
  use source-control-manager-internals; 
  export
    <vss-source-control-system>;
end module source-control-vss-backend;
