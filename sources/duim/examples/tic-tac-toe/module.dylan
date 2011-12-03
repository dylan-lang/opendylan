Module:       dylan-user
Author:       Jonathon Lee
Synopsis:     The classic game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module tic-tac-toe
  use common-dylan;
  use simple-random;
  use operating-system;
  use duim;

  export <ttt-frame>,
         play-tic-tac-toe;
end module tic-tac-toe;
