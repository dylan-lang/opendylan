Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Reversi game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module reversi
  use common-dylan;
  use simple-io;		// exported from common-dylan
  use simple-random;		// exported from common-dylan
  use operating-system;		// exported from system
  use file-system;		// exported from system
  use streams;			// exported from io
  use duim;

  export <reversi-frame>,
         play-reversi;
end module reversi;

