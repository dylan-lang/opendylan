Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library progress-stream-test-suite
  use common-dylan;
  use testworks;
  use io;
  use progress-stream;

  export progress-stream-test-suite;
end library progress-stream-test-suite;

define module progress-stream-test-suite
  use common-dylan;
  use testworks;
  use streams;
  use format;
  use progress-stream;

  export progress-stream-test-suite;
end module progress-stream-test-suite;
