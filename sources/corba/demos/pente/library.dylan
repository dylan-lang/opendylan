Module:    dylan-user
Author:    James Kirsch, Jason Trenouth
Synopsis:  Distributed Pente Game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-pente
  use common-dylan;
  use system;
  use duim;  
  use dylan-orb;
  use pente-skeletons;
  use network;
  use io;
  export pente;
end library corba-pente;

