Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// make-test-instance

define sideways method make-test-instance
    (class == <singleton>) => (object)
  make(<singleton>, object: #f)
end method make-test-instance;

define sideways method make-test-instance
    (class == <generic-function>) => (object)
  make(<generic-function>, required: 1)
end method make-test-instance;
