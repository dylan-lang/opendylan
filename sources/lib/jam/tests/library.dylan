Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library jam-test-suite
  use common-dylan;
  use testworks;
  use io;
  use system;
  use jam;

  export jam-test-suite;
end library jam-test-suite;

define module jam-test-suite
  use common-dylan;
  use testworks;
  use locators;
  use file-system;
  use streams;
  use threads;
  use jam;

  export jam-test-suite;
end module jam-test-suite;
