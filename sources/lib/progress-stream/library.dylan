Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2020 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library progress-stream
  use common-dylan;
  use io;

  export progress-stream;
end library;

define module progress-stream
  create
    <progress-stream>,
    stream-supports-show-progress?,
    show-progress;
end module;

define module progress-stream-internals
  use common-dylan;
  use streams;
  use streams-internals,
    import: { <file-stream>, stream-console?, write-fill };
  use progress-stream;
end module;
