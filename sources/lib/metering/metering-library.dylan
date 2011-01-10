Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library metering
  use functional-dylan;
  export metering;
end library;

define module metering
  use functional-dylan;
  export
    \with-metering, do-with-metering,
    metering-set-definer, make-metering-set;
end module;
