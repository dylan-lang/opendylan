Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library imported-dl
  use dylan;
  use c-ffi;
  export imported-dl;
end library;

define module imported-dl
  use dylan;
  use c-ffi;
  export
    dlopen, dlsym, dlclose, dlerror, $RTLD_LAZY, $RTDL_NOW;
end module;

// eof
