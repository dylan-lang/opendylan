Module:       dylan-user
Author:       James Kirsch
Synopsis:     Pente game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module pente
  use common-dylan;
  use simple-random;		// exported from common-dylan
  use operating-system;		// exported from system

  use duim;

  export <pente-frame>,
         play-pente;
end module pente;
