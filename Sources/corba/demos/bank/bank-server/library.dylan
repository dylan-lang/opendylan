Module:    dylan-user
Synopsis:  The CORBA server of the bank example.
Author:    Claudio Russo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library bank-server
  use functional-dylan;
  use io;
  use system;
  use dylan-orb;
  use bank-skeletons;
  use naming-client;
  use sql-odbc;
  use duim;
end library bank-server;



