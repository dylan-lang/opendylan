Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// make-test-instance

define function in-emulator? () => (emulator? :: <boolean>)
  //--- This hack may not always work if we fix up the emulator!
  type-union(<string>, <integer>) == <object>
end function in-emulator?;

define sideways method make-test-instance
    (class == <class>) => (object)
  if (in-emulator?())
    error("make(<class>) crashes in emulator, so switched off for now")
  else
    next-method()
  end
end method make-test-instance;

define sideways method make-test-instance
    (class == <singleton>) => (object)
  make(<singleton>, object: #f)
end method make-test-instance;

define sideways method make-test-instance
    (class == <generic-function>) => (object)
  make(<generic-function>, required: 1)
end method make-test-instance;
