Module:       dylan-user
Author:       James Kirsch
Synopsis:     Pente game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module pente
  use functional-dylan;
  use simple-format;		// exported from functional-dylan
  use simple-random;		// exported from functional-dylan
  use operating-system;		// exported from system

  use duim;

  export <pente-frame>,
         play-pente;
end module pente;
