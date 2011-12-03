Module:   dylan-user
Synopsis: Noddy status switching buttons demo
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module status-buttons
  use streams;
  use standard-io;
  use print;
  use format-out;
  use format;
  use duim;
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use common-dylan, exclude: { format-to-string };
  use simple-random;
  use status-icons;

end module status-buttons;
