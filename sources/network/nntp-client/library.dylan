Module:    dylan-user
Synopsis:  Thin wrapper around nntp
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library nntp-client
  use functional-dylan;
  use io;
  use network;

  export nntp-client;
end library nntp-client;
