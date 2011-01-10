Module:       dylan-user
Synopsis:     DUIM implementation of the game Tetris
Author:       Richard Tucker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module tetris
  use operating-system;
  use standard-io;
  use print;
  use format-out;
  use format;
  use duim;
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use functional-dylan;
  use simple-random;
end module tetris;
