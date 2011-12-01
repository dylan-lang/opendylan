Module:    dylan-user
Synopsis:  Thin wrapper around POP3
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library pop-client
  use common-dylan;
  use io;
  use network;

  export pop-client;
end library pop-client;
