Module:    dylan-user
Synopsis:  The CORBA client of the bank example.
Author:    Claudio Russo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library bank-client
  use common-dylan;
  use dylan-orb;
  use bank-stubs;
  use naming-client;
  use system;
  use io;
  use duim;
end library bank-client;
