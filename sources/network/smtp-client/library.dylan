Module:    dylan-user
Synopsis:  Thin wrapper around SMTP
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library smtp-client
  use common-dylan;
  use io;
  use network;
  use regular-expressions;

  export smtp-client;
end library smtp-client;
