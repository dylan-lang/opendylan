Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// some useful utilities

define generic printable-value (object) => (value);

define method printable-value (object) => (value)
  object
end method printable-value;
