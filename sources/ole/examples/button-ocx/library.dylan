Module:    dylan-user
Library:   button-ocx
Synopsis:  Demonstrate using a DUIM gadget as an OLE Control.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library button-ocx
  use dylan;
  use common-dylan;
  use DUIM;
  use DUIM-OLE-control;

  export button-ocx;
end library;

define module button-ocx
  use dylan;
  use common-dylan;
  use DUIM;
  use DUIM-OLE-control;
end module;
