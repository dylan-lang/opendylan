Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Mini-DUIM Othello game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library mini-othello
  use dylan;
  use functional-extensions;
  use format;
  use mini-duim;
    
  //--- For debugging
  use win32-kernel;

  export othello;
end library mini-othello;
